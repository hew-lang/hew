#![allow(
    deprecated,
    reason = "temporary named identity reconstruction migration seam"
)]

#[cfg(test)]
use super::drop_plan::ty_is_closure_pair_vec;
use super::{
    builtin_method_arg_is_move_ingress, classify_closure_pair_rhs, classify_dyn_trait_storage,
    cmp_select_by_signedness, context_reader_offset, describe_vec_element,
    dyn_rebind_source_binding, field_override_uses_record_field_drop, float_width,
    integer_bit_width, integer_signedness, is_self_expr, is_string_const_ty, machine_emit_type_id,
    mangle_layout_key, mangle_machine_step, monomorphic_user_record_key, numeric_method_op,
    numeric_method_signedness, option_payload_ty, runtime_symbol_for_call_expr, signed_min_value,
    ty_is_closure_pair, ty_is_generator_handle, ty_is_indirect_enum, ty_is_stream_handle,
    unary_op_label, unresolved_fn_sig_reason, user_record_layout_key, ActorStateLoadMode, BinaryOp,
    BindingId, Builder, BuiltinType, ChildKind, ClosurePairRhs, CmpPred, Disposition,
    FailClosedReason, FieldOffset, FloatWidth, HashMap, HashSet, HirExpr, HirExprKind, HirLiteral,
    HirStmtKind, HirVarSelfMethodTarget, Instr, IntArithOp, IntSignedness, IntentKind,
    MirDiagnostic, MirDiagnosticKind, MirStatement, NumericMethodFamily,
    PendingAffineCallConsumeArg, PendingAffineCallConsumeSite, Place, ProjectedPayloadOrigin,
    ProjectedPayloadRejectReason, ReleaseSymbolVerdict, ResolvedRef, ResolvedTy,
    RuntimeCallContext, SiteId, SuspendKind, Terminator, TrapKind, UnaryOp, ValueClass,
    VecElementRelease, FOR_ITER_CURSOR_NAME_PREFIX, SENTINEL_RECV_GEN_COMPANION_BINDING,
};
#[cfg(test)]
use super::{FieldLoadClass, PlaceProvenance, Projection, ValueProvenance};
use crate::model::ActorStateStoreHandoff;

mod runtime_builtins;

/// Compiler-owned primitive `Display::fmt` implementations live in the
/// catalog, not as emitted Hew impl bodies.  Static dispatch reaches here only
/// after the checker has selected the exact Display method declaration, so map
/// that declaration plus the substituted primitive type to its catalog call.
/// Other traits and user-defined `Display` impls continue through the
/// declaration-keyed impl registry below.
fn primitive_display_static_callee(
    target: &hew_types::CallTarget,
    receiver: &ResolvedTy,
    explicit_arg_count: usize,
) -> Option<&'static str> {
    let hew_types::CallTarget::StaticTraitMethod {
        declaring_trait,
        method,
    } = target
    else {
        return None;
    };
    if explicit_arg_count != 0
        || declaring_trait.full_path() != "std.builtins.Display"
        || method.full_path() != "std.builtins.Display::fmt"
    {
        return None;
    }
    match receiver {
        ResolvedTy::I8 | ResolvedTy::I16 | ResolvedTy::I32 => Some("to_string_i32"),
        ResolvedTy::I64 | ResolvedTy::Isize => Some("to_string_i64"),
        ResolvedTy::U8 => Some("to_string_u8"),
        ResolvedTy::U16 | ResolvedTy::U32 => Some("to_string_u32"),
        ResolvedTy::U64 | ResolvedTy::Usize => Some("to_string_u64"),
        ResolvedTy::F64 => Some("to_string_f64"),
        ResolvedTy::Bool => Some("to_string_bool"),
        ResolvedTy::Char => Some("to_string_char"),
        ResolvedTy::String => Some("to_string_str"),
        _ => None,
    }
}

/// The embedded `string` Display implementation is compiled as the catalog
/// identity conversion, not as a separately emitted impl body.  Imported stdlib
/// source can retain the exact implementation declaration in its checked call
/// target, so recognize that declaration directly rather than requiring a
/// linker symbol from the importing module's HIR item list.
fn stdlib_string_display_impl_callee(declaration: &hew_types::DefId) -> Option<&'static str> {
    (declaration.full_path()
        == "std.builtins.string::<impl std.builtins.Display for std.builtins.string>::fmt")
        .then_some("to_string_str")
}

/// Catalog display endpoints are compiler-selected ABI shims, not ordinary
/// source calls. Their catalogue identity authorizes the audited extern edge.
fn catalog_display_call_authority(callee: &str) -> crate::CallAuthority {
    hew_hir::stdlib_catalog::trusted_ffi_symbol_for_endpoint(callee)
        .map(|_| crate::CallAuthority::Extern)
        .unwrap_or_default()
}

/// Project a checker-validated catalog endpoint onto its compiler-owned MIR
/// operation. The caller must supply the endpoint carried by
/// `CallTarget::Builtin`; arbitrary source or linker spellings never reach
/// this discriminator.
fn compiler_builtin_call_authority(endpoint: &str) -> Option<crate::CallAuthority> {
    use crate::IdentityAggregateKind as Kind;

    let kind = match hew_types::runtime_call::RuntimeCallFamily::from_checker_signature(endpoint) {
        Some(hew_types::runtime_call::RuntimeCallFamily::NodeId) => Kind::NodeId,
        _ => match endpoint {
            "hew_node_id_display" => Kind::NodeIdDisplay,
            "hew_location_node_id" => Kind::LocationNodeId,
            "hew_location_slot" => Kind::LocationSlot,
            "hew_location_incarnation" => Kind::LocationIncarnation,
            "hew_location_display" => Kind::LocationDisplay,
            "hew_remote_pid_location" => Kind::RemotePidLocation,
            "hew_remote_pid_node_id" => Kind::RemotePidNodeId,
            "hew_remote_pid_slot" => Kind::RemotePidSlot,
            "hew_remote_pid_incarnation" => Kind::RemotePidIncarnation,
            "hew_remote_pid_display" => Kind::RemotePidDisplay,
            _ => return None,
        },
    };
    Some(crate::CallAuthority::Compiler(
        crate::CompilerCallKind::IdentityAggregate(kind),
    ))
}

/// The stdlib's generic `Iterator::next` body reaches MIR as static trait
/// dispatch after its `I` parameter is monomorphised. `VecIter<T>` is a
/// compiler-owned cursor, though: its concrete next operation is the same
/// checked clone-out state machine HIR uses for a direct `iter.next()` call.
///
/// Keep this discriminator declaration-keyed and builtin-tagged. A user type
/// named `VecIter`, a different `next` trait, or an iterator with arguments
/// must continue through ordinary impl dispatch rather than borrowing this
/// runtime protocol.
fn builtin_vec_iter_static_next_element<'a>(
    target: &hew_types::CallTarget,
    receiver: &'a ResolvedTy,
    explicit_arg_count: usize,
) -> Option<&'a ResolvedTy> {
    let hew_types::CallTarget::StaticTraitMethod {
        declaring_trait,
        method,
    } = target
    else {
        return None;
    };
    if explicit_arg_count != 0
        || declaring_trait.full_path() != "std.builtins.Iterator"
        || method.full_path() != "std.builtins.Iterator::next"
    {
        return None;
    }
    let ResolvedTy::Named {
        args,
        builtin: Some(BuiltinType::VecIter),
        ..
    } = receiver
    else {
        return None;
    };
    (args.len() == 1).then(|| &args[0])
}

pub(super) fn binding_seeds_drop_elaboration(
    ty: &ResolvedTy,
    type_classes: &hew_hir::TypeClassTable,
) -> bool {
    !super::drop_plan::ty_is_nonowning_handle_leaf(ty)
        && ValueClass::of_ty(ty, type_classes) != ValueClass::BitCopy
}

#[cfg(test)]
mod builtin_vec_iter_static_next_tests {
    use super::*;
    use hew_hir::ScopeId;

    fn iterator_next_target() -> hew_types::CallTarget {
        hew_types::CallTarget::static_trait(
            hew_types::DefId::legacy_reconstruct_from_full_path("std.builtins.Iterator"),
            hew_types::DefId::legacy_reconstruct_from_full_path("std.builtins.Iterator::next"),
        )
    }

    #[test]
    fn builtin_vec_iter_next_requires_exact_builtin_and_declaration_identities() {
        let builtin_cursor =
            ResolvedTy::named_builtin("VecIter", BuiltinType::VecIter, vec![ResolvedTy::I64]);
        assert_eq!(
            builtin_vec_iter_static_next_element(&iterator_next_target(), &builtin_cursor, 0),
            Some(&ResolvedTy::I64),
            "the builtin Iterator::next identity selects the VecIter state machine"
        );

        let user_spoof = ResolvedTy::named_user("VecIter", vec![ResolvedTy::I64]);
        assert!(
            builtin_vec_iter_static_next_element(&iterator_next_target(), &user_spoof, 0).is_none(),
            "a user VecIter spelling must not acquire the builtin cursor protocol"
        );

        let other_iterator_method = hew_types::CallTarget::static_trait(
            hew_types::DefId::legacy_reconstruct_from_full_path("std.builtins.Iterator"),
            hew_types::DefId::legacy_reconstruct_from_full_path("std.builtins.Iterator::peek"),
        );
        assert!(
            builtin_vec_iter_static_next_element(&other_iterator_method, &builtin_cursor, 0)
                .is_none(),
            "a different Iterator method must remain ordinary static dispatch"
        );
    }

    #[test]
    fn var_self_writeback_preserves_the_receiver_declaration_scope() {
        let binding = BindingId(700);
        let outer_scope = ScopeId(30);
        let loop_scope = ScopeId(31);
        let mut builder = Builder {
            active_scopes: vec![outer_scope],
            ..Builder::default()
        };
        builder.binding_locals.insert(binding, Place::Local(1));
        builder.record_binding_scope(binding);

        builder.active_scopes.push(loop_scope);
        builder.restore_var_self_receiver_binding(binding, "inner", &ResolvedTy::I64, SiteId(700));

        assert_eq!(
            builder.binding_scope[&binding], outer_scope,
            "a write-back is a new value generation, not a new lexical declaration"
        );
    }
}

/// The specialised terminal either does not apply, or it owns the call site
/// completely (including a possible diagnostic/lowering failure).
enum VecIterStaticNextLowering {
    NotApplicable,
    Lowered(Option<Place>),
}

impl Builder {
    /// Whether a functional-update CARRY of a non-overridden field of type
    /// `ty` soundly transfers that field's ownership out of the consumed base.
    ///
    /// The carry is a shallow `RecordFieldLoad` into the result's
    /// `RecordInit`. It is sound only when the field's whole drop obligation
    /// travels with that one shallow read:
    ///   * `BitCopy` / `View` — no heap ownership; nothing to transfer.
    ///   * a single-pointer COW / handle leaf (string, bytes, `Vec`,
    ///     `HashMap`, `HashSet`, `Generator`) — `project_field_inline_drop_-
    ///     symbol` is `Wired`, so the one allocation is released exactly once.
    ///   * an owned user record **each of whose fields is itself carry-sound by
    ///     this same rule**. A record is not an opaque transfer boundary: its
    ///     nested fields must all have a release path after the shallow read.
    ///   * a heap-owning tuple **each of whose elements is itself carry-sound
    ///     by this same rule**. The tuple is not a transfer boundary in its
    ///     own right: a tuple element whose bare form has no sound carry has
    ///     no sound carry inside a tuple either. Admitting one — an
    ///     `Option<owned>` element, say — excludes the consumed base from its
    ///     `RecordInPlace` drop without the result taking over the element's
    ///     obligation, and the base's payload is never released (a leak, pinned
    ///     by `reject_carry_tuple_of_option_field`).
    ///
    /// Every other owned field type fails closed: a closure / `fn` /
    /// trait-object capture env, an `@resource` / cancellation-token / task
    /// handle, a bare `Option<owned>` or enum-with-heap, and any
    /// `Unknown`-class owned Named type. Lifting a specific type's carry is
    /// tracked in hew-lang/hew#2207.
    fn carry_transfers_field_ownership(&self, ty: &ResolvedTy) -> bool {
        if matches!(
            ValueClass::of_ty(ty, &self.type_classes),
            ValueClass::BitCopy | ValueClass::View
        ) {
            return true;
        }
        if matches!(
            self.project_field_inline_drop_symbol(ty),
            ReleaseSymbolVerdict::Wired(_)
        ) {
            return true;
        }
        if self.is_owned_aggregate_record_ty(ty) {
            let Some(key) = user_record_layout_key(ty) else {
                return false;
            };
            let Some(fields) = self.lookup_record_field_order(&key) else {
                return false;
            };
            return fields.iter().all(|(_, field_ty)| {
                self.carry_transfers_field_ownership(&self.subst_ty(field_ty))
            });
        }
        let ResolvedTy::Tuple(elems) = ty else {
            return false;
        };
        crate::lower::drop_plan::ty_is_heap_owning_tuple(
            ty,
            &self.record_field_orders,
            &self.enum_layouts,
            &self.lifecycle_registry,
        ) && elems
            .iter()
            .all(|elem| self.carry_transfers_field_ownership(elem))
    }

    /// Lower the exact builtin `Iterator::next` dispatch for `VecIter<T>`.
    ///
    /// HIR expands direct `cursor.next()` syntax before generic functions are
    /// specialised. A call originating in `std::iter::fold<I: Iterator>` is
    /// necessarily still a static-trait call in HIR, so after `I = VecIter<T>`
    /// substitution MIR materialises the identical cursor state machine here:
    /// compare `idx` with the snapshot Vec length, clone the current element
    /// into `Some`, advance `idx`, or construct `None`.
    #[expect(
        clippy::too_many_lines,
        reason = "one cursor advancement must emit a contiguous CFG: split helpers would obscure the ownership and block transitions"
    )]
    fn lower_builtin_vec_iter_static_next(
        &mut self,
        receiver: &HirExpr,
        elem_ty: &ResolvedTy,
        ret_ty: &ResolvedTy,
        site: SiteId,
    ) -> Option<Place> {
        if let Err(reason) = self.validate_collection_clone_value(elem_ty) {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("`VecIter<{}>` clone-out", elem_ty.user_facing()),
                    site,
                },
                note: format!(
                    "`VecIter.next()` must clone each element into an independent owner, \
                     but {reason}; MIR refuses to lower the generic Iterator dispatch"
                ),
            });
            return None;
        }

        let HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(binding),
            name,
        } = &receiver.kind
        else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::UnsupportedNode {
                    reason: "builtin VecIter Iterator.next receiver is not a binding".to_string(),
                },
                note: "the builtin cursor state machine mutates the receiver's existing local slot"
                    .to_string(),
            });
            return None;
        };
        let Some(cursor) = self.binding_locals.get(binding).copied() else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::UnresolvedPlace {
                    binding: *binding,
                    name: name.clone(),
                    site: receiver.site,
                },
                note: "builtin VecIter Iterator.next receiver has no MIR place".to_string(),
            });
            return None;
        };
        // `VecIter::next` advances the cursor in place; it does not transfer
        // the cursor out of its binding.  The generic `var self` HIR carrier
        // models an ordinary method's dual-return write-back as Consume, but
        // this builtin intercept replaces that carrier with field mutation, so
        // record the read directly and keep the VecIter scope owner live.
        self.statements.push(MirStatement::Use {
            binding: *binding,
            name: name.clone(),
            site: receiver.site,
            ty: self.subst_ty(&receiver.ty),
            intent: IntentKind::Read,
        });
        let vec_ty = ResolvedTy::named_builtin("Vec", BuiltinType::Vec, vec![elem_ty.clone()]);
        let vec = self.alloc_local(vec_ty);
        self.push_instr(Instr::RecordFieldLoad {
            record: cursor,
            field_offset: FieldOffset(0),
            dest: vec,
        });
        let idx = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::RecordFieldLoad {
            record: cursor,
            field_offset: FieldOffset(1),
            dest: idx,
        });

        let len = self.alloc_local(ResolvedTy::I64);
        let after_len = self.alloc_block();
        self.finish_current_block(Terminator::Call {
            callee: "hew_vec_len".to_string(),
            authority: crate::CallAuthority::Runtime(
                hew_types::runtime_call::RuntimeCallFamily::VecLen,
            ),
            args: vec![vec],
            dest: Some(len),
            next: after_len,
        });
        self.start_block(after_len);

        let exhausted = self.alloc_local(ResolvedTy::Bool);
        self.push_instr(Instr::IntCmp {
            pred: CmpPred::SignedGreaterEq,
            lhs: idx,
            rhs: len,
            dest: exhausted,
        });
        let none_bb = self.alloc_block();
        let some_bb = self.alloc_block();
        let join_bb = self.alloc_block();
        let result = self.alloc_local(ret_ty.clone());
        self.finish_current_block(Terminator::Branch {
            cond: exhausted,
            then_target: none_bb,
            else_target: some_bb,
        });

        self.start_block(none_bb);
        let Place::Local(result_local) = result else {
            unreachable!("alloc_local returns Place::Local");
        };
        self.push_instr(Instr::ConstI64 {
            dest: Place::EnumTag(result_local),
            value: 1,
        });
        self.finish_current_block(Terminator::Goto { target: join_bb });

        self.start_block(some_bb);
        let after_get = self.alloc_block();
        self.finish_current_block(Terminator::Call {
            callee: "hew_vec_get_clone".to_string(),
            authority: crate::CallAuthority::Runtime(
                hew_types::runtime_call::RuntimeCallFamily::VecGet(
                    hew_types::runtime_call::VecGetElem::Clone,
                ),
            ),
            args: vec![vec, idx],
            dest: Some(result),
            next: after_get,
        });
        self.start_block(after_get);
        let one = self.alloc_local(ResolvedTy::I64);
        self.push_instr(Instr::ConstI64 {
            dest: one,
            value: 1,
        });
        let next_idx = self.alloc_local(ResolvedTy::I64);
        let overflow = self.alloc_local(ResolvedTy::Bool);
        self.push_instr(Instr::IntArithChecked {
            op: IntArithOp::Add,
            signed: IntSignedness::Signed,
            dest: next_idx,
            lhs: idx,
            rhs: one,
            overflow_flag: overflow,
        });
        let overflow_bb = self.alloc_block();
        let advance_bb = self.alloc_block();
        self.finish_current_block(Terminator::Branch {
            cond: overflow,
            then_target: overflow_bb,
            else_target: advance_bb,
        });
        self.start_block(overflow_bb);
        self.finish_current_block(Terminator::Trap {
            kind: TrapKind::IntegerOverflow,
        });
        self.start_block(advance_bb);
        self.push_instr(Instr::RecordFieldStore {
            record: cursor,
            field_offset: FieldOffset(1),
            src: next_idx,
        });
        self.finish_current_block(Terminator::Goto { target: join_bb });

        self.start_block(join_bb);
        Some(result)
    }

    /// Recognises only the exact builtin `VecIter<T>::Iterator::next`
    /// terminal after substitution. Every adapter stays on ordinary
    /// declaration-indexed static dispatch.
    fn lower_builtin_vec_iter_static_next_if_applicable(
        &mut self,
        receiver_type_param: &str,
        receiver: &HirExpr,
        call_target: &hew_types::CallTarget,
        args: &[HirExpr],
        ret_ty: &ResolvedTy,
        site: SiteId,
    ) -> VecIterStaticNextLowering {
        let Some(concrete_ty) = self.subst.get(receiver_type_param).cloned() else {
            return VecIterStaticNextLowering::NotApplicable;
        };
        let Some(elem_ty) =
            builtin_vec_iter_static_next_element(call_target, &concrete_ty, args.len())
        else {
            return VecIterStaticNextLowering::NotApplicable;
        };
        VecIterStaticNextLowering::Lowered(self.lower_builtin_vec_iter_static_next(
            receiver,
            elem_ty,
            &self.subst_ty(ret_ty),
            site,
        ))
    }

    /// The `let` / `var` binder's combined seed-and-provenance gate.
    ///
    /// `Some(warrant)` when this binding earns a scope-exit owner: its type
    /// seeds drop elaboration, the same iteration will wire its backend slot,
    /// and the provenance question over its initializer came back not-proven-
    /// foreign. `None` withholds — and because the whole registration block
    /// hangs off this `Option`, a withheld binder also skips the generator /
    /// stream / `VecIter` per-scope-exit taggings that would otherwise schedule
    /// their own release for a value this program does not own.
    ///
    /// The warrant it returns is the only thing that will satisfy
    /// [`Builder::register_owned_local`], so the gate and the mint cannot drift:
    /// there is no path from "this type is not `BitCopy`" to an owner entry that
    /// does not pass through here.
    pub(crate) fn let_binder_owner_warrant(
        &mut self,
        binding: BindingId,
        value: &HirExpr,
        binding_ty: &ResolvedTy,
        slot_is_wired: bool,
    ) -> Option<super::OwnerMintWarrant> {
        if !self.binding_seeds_drop_elaboration(binding_ty) || !slot_is_wired {
            return None;
        }
        let warrant = self.owner_warrant_for_initializer(binding, value, binding_ty);
        (!warrant.withholds_mint()).then_some(warrant)
    }

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
    /// indirect enum also has `named_elem_carries_drop_obligation == false` (the
    /// heap-ownership authority is indirection-blind), but a HEAP-payload one
    /// (`A(string)`) has `named_elem_carries_drop_obligation == true` and would otherwise fall
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
        // `false` at the `named_elem_carries_drop_obligation` check below; this guard also
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
        let is_enum = self.enum_layouts.iter().any(|el| el.name == key);
        let is_record = self.lookup_record_field_order(&key).is_some();
        if !is_enum && !is_record {
            return false;
        }
        // ONLY a genuinely heap-owning element is an owned-Vec element, decided
        // by the `named_elem_carries_drop_obligation` authority (NOT by `ValueClass::BitCopy`,
        // which finalises records only). A heap-free record (e.g.
        // `type Point { x: i64; y: i64 }`, which is `BitCopy`) OR a heap-free
        // direct enum (e.g. `enum Colour { Red; Green; Blue }`, which is NOT
        // `BitCopy`) owns no heap and stays on the plain-Vec path
        // (`is_plain_vec_element`); harvesting it would mis-route its Vec's
        // element loads through the owned getter (which reads an owned
        // descriptor the plain Vec never carries). Check field/variant
        // heap-ownership via the record/enum registries.
        if !self.named_elem_carries_drop_obligation(elem) {
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
        let is_enum = self.enum_layouts.iter().any(|el| el.name == key);
        if is_enum {
            // Enums are gated by their own EnumInPlace drop path; record the
            // mangled enum key so a value of the enum admits as CowValue too.
            self.vec_owned_element_keys.insert(key);
        } else {
            // Use the record-layout-key form for records (matches
            // `user_record_layout_key` consulted by the W3.029 escape hatch).
            self.vec_owned_element_keys.insert(key);
        }
    }

    /// True when a `ResolvedTy` carries a drop obligation — transitively owns
    /// heap OR contains a registered closeable `#[resource]`. Thin adapter over
    /// the single `crate::model::ty_carries_drop_obligation` authority (record
    /// fields via `record_field_orders`, enum/machine variant payloads via
    /// `enum_layouts`, one builtin leaf set, close contracts via the lifecycle
    /// registry). The owned-Vec element harvest consults this so an all-BitCopy
    /// record/enum (which is `Copy` and uses the `BitCopy` `_layout` path) is
    /// NOT treated as an owned-Vec element, while a `CancellationToken`/
    /// `Generator`-bearing element — or a scalar-field `#[resource]` record,
    /// whose element drop must run `close` — correctly is. Same verdict the
    /// codegen owned-Vec walker reaches, so getter, constructor, and release
    /// agree (`dedup-semantic-boundary`).
    pub(super) fn named_elem_carries_drop_obligation(&self, ty: &ResolvedTy) -> bool {
        crate::model::ty_carries_drop_obligation_mir(
            ty,
            &self.record_field_orders,
            &self.enum_layouts,
            self.type_classes.lifecycle_registry(),
        )
    }

    /// Lower a consuming `VecIter` `vec`-field source that reads a bare actor
    /// state field: load the field's handle (an alias read — the take-all call
    /// arg is the load's only use, so the state-load classifier keeps it a
    /// bare `Borrowed` load), then move the buffer into a fresh vec via
    /// `hew_vec_take_all`. The returned place owns the taken vec; the state
    /// slot keeps its handle, now a valid empty vec with the same stamped
    /// element descriptor (dogfood F3 — see
    /// [`Builder::vec_field_src_consumes_bare_actor_state_field`]).
    fn lower_vec_take_all_from_state_field(&mut self, fexpr: &HirExpr) -> Option<Place> {
        let src = self.lower_value(fexpr)?;
        let taken = self.alloc_local(self.subst_ty(&fexpr.ty));
        let next = self.alloc_block();
        self.finish_current_block(Terminator::Call {
            callee: "hew_vec_take_all".to_string(),
            authority: crate::CallAuthority::Runtime(
                hew_types::runtime_call::RuntimeCallFamily::VecTakeAll,
            ),
            args: vec![src],
            dest: Some(taken),
            next,
        });
        self.start_block(next);
        Some(taken)
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
        let key = if args.is_empty() {
            name.clone()
        } else {
            mangle_layout_key(name, args)
        };
        self.lookup_record_field_order(&key).map(|_| key)
    }

    fn payload_enum_layout_key_for_eq(&self, ty: &ResolvedTy) -> Option<String> {
        let ResolvedTy::Named { name, args, .. } = ty else {
            return None;
        };
        let key = if args.is_empty() {
            name.clone()
        } else {
            mangle_layout_key(name, args)
        };
        self.enum_layouts
            .iter()
            .find(|layout| layout.name == key)
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
        let key = if args.is_empty() {
            name.clone()
        } else {
            mangle_layout_key(name, args)
        };
        self.enum_layouts
            .iter()
            .find(|layout| layout.name == key)
            .map(|layout| layout.name.clone())
    }

    fn is_structural_eq_comparison(&self, lhs_ty: &ResolvedTy, rhs_ty: &ResolvedTy) -> bool {
        if lhs_ty == rhs_ty && matches!(lhs_ty, ResolvedTy::Tuple(_)) {
            return true;
        }
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
    pub(super) fn is_owned_vec_element(&self, elem_ty: &ResolvedTy) -> bool {
        if self
            .lifecycle_registry
            .opaque_resource_for_ty(elem_ty)
            .is_some()
        {
            return true;
        }
        match elem_ty {
            // A tuple element is owned when any field transitively owns heap.
            // Use `named_elem_carries_drop_obligation` (which consults
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
            ResolvedTy::Tuple(elems) => elems
                .iter()
                .any(|e| self.named_elem_carries_drop_obligation(e)),
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
            // A trait-object slot likewise owns its two-word `HeapBoxed` payload
            // under a drop-only element descriptor.
            ResolvedTy::TraitObject { .. }
            | ResolvedTy::Named {
                builtin:
                    Some(
                        hew_types::BuiltinType::HashMap
                        | hew_types::BuiltinType::HashSet
                        | hew_types::BuiltinType::Rc
                        | hew_types::BuiltinType::Weak
                        | hew_types::BuiltinType::Sender
                        | hew_types::BuiltinType::Receiver,
                    ),
                ..
            } => true,
            ResolvedTy::Named {
                builtin: Some(hew_types::BuiltinType::Vec),
                args,
                ..
            } => !args.first().is_some_and(|e| {
                matches!(e, ResolvedTy::Function { .. } | ResolvedTy::Closure { .. })
            }),
            ResolvedTy::Named {
                name,
                args,
                builtin,
                ..
            } => {
                let key = match builtin {
                    Some(
                        cursor @ (hew_types::BuiltinType::VecIter
                        | hew_types::BuiltinType::HashMapIter),
                    ) => hew_hir::synthetic_cursor_layout_key(*cursor, args),
                    _ if args.is_empty() => Some(name.clone()),
                    _ => Some(mangle_layout_key(name, args)),
                };
                key.is_some_and(|key| self.vec_owned_element_keys.contains(&key))
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
            | ResolvedTy::Task(_)
            | ResolvedTy::TypeParam { .. } => false,
        }
    }

    /// The owned-locals seed authority: does a binding of type `ty` oblige
    /// scope-exit drop elaboration? `true` admits the binding into the
    /// `owned_locals` ledger that the ownership finalizers, owner `Mint`
    /// publication, and `unsupported_vec_element_diagnostics` read; the exit
    /// plans themselves derive from the minted owners' event replay.
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
    /// the seed side leaves a moved-out binding in `owned_locals`, and its
    /// scope-exit owner then releases a moved-out value (an
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
            || self.record_with_ready_inline_enum_owned_field(ty)
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
                let key = if args.is_empty() {
                    name.clone()
                } else {
                    mangle_layout_key(name, args)
                };
                self.record_field_orders.contains_key(&key)
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
    /// covers records only). Callers pair it with the `named_elem_carries_drop_obligation`
    /// authority so a heap-owning direct enum still routes owned.
    fn ty_is_direct_enum_element(&self, elem_ty: &ResolvedTy) -> bool {
        let ResolvedTy::Named { name, args, .. } = elem_ty else {
            return false;
        };
        let key = if args.is_empty() {
            name.clone()
        } else {
            mangle_layout_key(name, args)
        };
        self.enum_layouts
            .iter()
            .any(|el| !el.is_indirect && el.name == key)
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

    /// True when a Vec element descriptor is move-only. Push/set transfer the
    /// source generation into the collection; copy-out operations are refused.
    pub(super) fn vec_receiver_has_drop_only_element(&self, vec_ty: &ResolvedTy) -> bool {
        let ResolvedTy::Named {
            args,
            builtin: Some(hew_types::BuiltinType::Vec),
            ..
        } = self.subst_ty(vec_ty)
        else {
            return false;
        };
        args.first().is_some_and(|elem| {
            matches!(self.subst_ty(elem), ResolvedTy::TraitObject { .. })
                || matches!(
                    self.subst_ty(elem),
                    ResolvedTy::Named {
                        builtin: Some(hew_types::BuiltinType::Receiver),
                        ..
                    }
                )
                || self.collection_clone_affine_blocker(elem).is_some()
        })
    }

    pub(super) fn reject_drop_only_vec_operation(
        &mut self,
        operation: &str,
        site: SiteId,
    ) -> Option<Place> {
        self.diagnostics.push(MirDiagnostic {
            kind: MirDiagnosticKind::NotYetImplemented {
                construct: format!("drop-only `Vec` element operation `{operation}`"),
                site,
            },
            note: "this Vec element has a drop callback but no semantic clone: the operation \
                   would create a second owner. Use push/pop/remove or consuming iteration \
                   to transfer the value instead."
                .to_string(),
        });
        None
    }

    /// Prove that a concrete collection payload has a total semantic clone and
    /// inverse drop.
    ///
    /// The type checker can decide this directly for monomorphic `VecIter<E>`
    /// sites.  A generic body is checked before `T` is substituted, however,
    /// so its genuine type parameter is deliberately deferred to this MIR
    /// boundary.  `Builder::subst_ty` supplies the concrete instantiation and
    /// this proof consumes the same structural clone authority used by actor
    /// snapshots and owned call carriers.
    ///
    /// `record_field_orders` is the builder's complete monomorphic record
    /// registry (including post-HIR-mono generic layouts).  Repackage it as the
    /// classifier's descriptor view rather than maintaining a second clone
    /// classifier here.  Field names are carried only to preserve the shared
    /// `RecordLayout` shape; clone totality depends on the field types.
    fn validate_collection_clone_value(&self, value_ty: &ResolvedTy) -> Result<(), String> {
        let concrete = self.subst_ty(value_ty);
        let mut visiting_markers = HashSet::new();
        if let Some(blocker) =
            self.collection_clone_affine_marker_blocker(&concrete, &mut visiting_markers)
        {
            return Err(blocker);
        }
        let record_layouts: Vec<crate::model::RecordLayout> = self
            .record_field_orders
            .iter()
            // A record descriptor must carry at least one stored field:
            // zero-field named records are rejected upstream, while positional
            // records now enter this table with authoritative payload types and
            // synthetic ordinal names. Excluding an empty entry prevents stale
            // or malformed metadata from manufacturing clone totality.
            .filter(|(_, fields)| !fields.is_empty())
            .map(|(name, fields)| crate::model::RecordLayout {
                name: name.clone(),
                field_tys: fields.iter().map(|(_, ty)| ty.clone()).collect(),
                field_names: fields.iter().map(|(field, _)| field.clone()).collect(),
            })
            .collect();
        let plan = crate::state_clone::classify_value_snapshot_plan_with_lifecycle_registry(
            &concrete,
            &record_layouts,
            &self.enum_layouts,
            &self.opaque_handle_names,
            &self.lifecycle_registry,
        )
        .map_err(|error| error.to_string())?;
        match plan.is_clone_total(
            &record_layouts,
            &self.enum_layouts,
            &self.opaque_handle_names,
            &self.lifecycle_registry,
        ) {
            Ok(true) => Ok(()),
            Ok(false) => Err(format!(
                "`{}` contains a drop-only or ownership-opaque value",
                concrete.user_facing()
            )),
            Err(error) => Err(error.to_string()),
        }
    }

    /// Canonical transitive affine capability used by every Vec copy/move
    /// boundary. A non-`None` result means the element may move but may not be
    /// cloned into a second owner.
    pub(super) fn collection_clone_affine_blocker(&self, value_ty: &ResolvedTy) -> Option<String> {
        let concrete = self.subst_ty(value_ty);
        self.collection_clone_affine_marker_blocker(&concrete, &mut HashSet::new())
    }

    /// Return the first non-cloneable resource / linear value reachable from a
    /// collection clone value.
    ///
    /// `state_clone` intentionally classifies a field-bearing non-opaque
    /// resource record structurally as `UserRecord` before consulting its
    /// opaque-handle resource registry: actor-state snapshot planning needs
    /// the record's field drop spine. Collection clone/get operations have a
    /// stricter contract, though — they must duplicate the WHOLE stored value,
    /// and an affine/linear user record has no semantic clone even when every
    /// field happens to be clone-total. Conjoin the shared structural proof with HIR's
    /// authoritative type-class marker, descending through the concrete
    /// record/enum layouts so a marker cannot hide inside an unmarked wrapper.
    ///
    /// A small closed builtin exception set has an already-ratified bit-copy or
    /// retain clone despite its representation marker (actor refs and
    /// Rc/Weak). Every other Resource/Linear marker is a veto, including
    /// field-bearing builtin resource records such as `MonitorRef`.
    #[expect(
        clippy::too_many_lines,
        reason = "one exhaustive recursive type-shape proof keeps the marker veto and every descent edge auditable together"
    )]
    fn collection_clone_affine_marker_blocker(
        &self,
        ty: &ResolvedTy,
        visiting: &mut HashSet<String>,
    ) -> Option<String> {
        let concrete = self.subst_ty(ty);
        match &concrete {
            ResolvedTy::Tuple(items) => items
                .iter()
                .find_map(|item| self.collection_clone_affine_marker_blocker(item, visiting)),
            ResolvedTy::Array(elem, _) => {
                self.collection_clone_affine_marker_blocker(elem, visiting)
            }
            ResolvedTy::Named {
                name,
                args,
                builtin,
                ..
            } => {
                let builtin_has_ratified_clone =
                    builtin.is_some_and(BuiltinType::is_affine_clone_terminal);
                if builtin_has_ratified_clone {
                    // These wrappers are terminal clone leaves. Actor refs
                    // bit-copy/erase their protocol identity argument. Rc/Weak
                    // and Sender retain/clone the outer shared handle and do
                    // not recursively clone their payload/protocol tag.
                    return None;
                }
                match hew_hir::lookup_type_marker_for_ty(&concrete, &self.type_classes) {
                    Some(hew_hir::ResourceMarker::Resource) => {
                        return Some(format!(
                            "resource `{}` has an affine close contract and no \
                                 semantic clone",
                            concrete.user_facing()
                        ));
                    }
                    Some(hew_hir::ResourceMarker::Linear) => {
                        return Some(format!(
                            "linear value `{}` must be consumed exactly once and \
                                 has no semantic clone",
                            concrete.user_facing()
                        ));
                    }
                    Some(hew_hir::ResourceMarker::None | hew_hir::ResourceMarker::BitCopy)
                    | None => {}
                }

                let key = if args.is_empty() {
                    name.clone()
                } else {
                    mangle_layout_key(name, args)
                };
                let visit_key = format!("named:{key}");
                if !visiting.insert(visit_key.clone()) {
                    return None;
                }
                let record_fields = self
                    .lookup_record_field_order(&key)
                    .or_else(|| self.lookup_record_field_order(name))
                    .cloned();
                let enum_fields = crate::model::find_enum_layout(name, args, &self.enum_layouts)
                    .map(|layout| {
                        layout
                            .variants
                            .iter()
                            .flat_map(|variant| variant.field_tys.iter().cloned())
                            .collect::<Vec<_>>()
                    });
                let blocker = if let Some(fields) = record_fields {
                    fields.iter().find_map(|(_, field_ty)| {
                        self.collection_clone_affine_marker_blocker(field_ty, visiting)
                    })
                } else if let Some(fields) = enum_fields {
                    fields.iter().find_map(|field_ty| {
                        self.collection_clone_affine_marker_blocker(field_ty, visiting)
                    })
                } else {
                    // Builtin containers and enum-like constructors do not
                    // have user record layouts in `record_field_orders`; their
                    // type arguments are their clone-relevant payloads.
                    args.iter()
                        .find_map(|arg| self.collection_clone_affine_marker_blocker(arg, visiting))
                };
                visiting.remove(&visit_key);
                blocker
            }
            ResolvedTy::TypeParam { .. }
            | ResolvedTy::String
            | ResolvedTy::Bytes
            | ResolvedTy::CancellationToken
            | ResolvedTy::Slice(_)
            | ResolvedTy::Function { .. }
            | ResolvedTy::Closure { .. }
            | ResolvedTy::Pointer { .. }
            | ResolvedTy::Borrow { .. }
            | ResolvedTy::TraitObject { .. }
            | ResolvedTy::Task(_)
            | ResolvedTy::I8
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
            | ResolvedTy::Unit
            | ResolvedTy::Never => None,
        }
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
            && !self.named_elem_carries_drop_obligation(elem);
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
        if let Some(layout) = self.enum_layouts.iter().find(|el| el.name == key) {
            return Some(layout.is_indirect);
        }
        if self.lookup_record_field_order(&key).is_some() {
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
                let is_for_await_handle_cursor = ty_is_stream_handle(&binding_ty)
                    && binding.name.starts_with(FOR_ITER_CURSOR_NAME_PREFIX);
                if is_for_await_handle_cursor {
                    // The desugared cursor's owner is bound below, after its
                    // destination slot and lexical scope are known. Do not
                    // also mint a provisional owner for the RHS transient:
                    // that would turn the one move into two live Stream
                    // owners and make aggregate extraction fail closed.
                    self.suppress_typed_produced_owner_sites.insert(value.site);
                }
                let diag_len_before_value = self.diagnostics.len();
                let value_place = self.lower_let_value(binding.id, value);
                self.suppress_typed_produced_owner_sites.remove(&value.site);
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
                // Establish the binding's canonical backend storage before
                // minting ownership. The former order registered ordinary let
                // owners against the `Local(0)` fallback, then allocated their
                // real slot after the mint; produced-value handoffs consequently
                // left both the intermediate and final Vec/record slots in the
                // exit template. Storage-first construction gives every OwnerId
                // its real immutable Place from birth.
                if !pending {
                    if let Some(src) = value_place {
                        let binding_place = match src {
                            Place::DuplexHandle(_)
                            | Place::SendHalf(_)
                            | Place::RecvHalf(_)
                            | Place::LambdaActorHandle(_)
                            | Place::ActorHandle(_) => Some(src),
                            Place::Local(n) if self.tuple_decomp.contains_key(&n) => Some(src),
                            Place::Local(_) | Place::ReturnSlot => {
                                Some(self.alloc_local(binding_ty.clone()))
                            }
                            Place::MachineTag(_)
                            | Place::MachineVariant { .. }
                            | Place::EnumTag(_)
                            | Place::EnumVariant { .. } => None,
                        };
                        if let Some(place) = binding_place {
                            self.binding_locals.insert(binding.id, place);
                        }
                    }
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
                self.push_bind_statement(
                    binding.id,
                    binding.name.clone(),
                    value.site,
                    binding_ty.clone(),
                );
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
                let borrowed_runtime_result = value_place
                    .is_some_and(|place| self.borrowed_runtime_result_places.contains(&place));
                let ordinary_owner_warrant = if !dyn_owned
                    && !borrowed_runtime_result
                    && self.vec_iter_cursor_release_symbol(&binding_ty).is_none()
                {
                    self.let_binder_owner_warrant(
                        binding.id,
                        value,
                        &binding_ty,
                        pending || value_place.is_some(),
                    )
                } else {
                    None
                };
                if dyn_owned {
                    // dyn-trait owned local: classify storage from the RHS
                    // expression shape and push into `owned_locals` with the
                    // actual `TraitObject` type so the owner's `DropRecipe`
                    // reaches the dyn-trait arm of `drop_kind_for`.
                    // Fail-closed if classification
                    // returns `Err`.
                    match classify_dyn_trait_storage(value, &self.dyn_trait_storage) {
                        Ok(storage) => {
                            self.dyn_trait_storage.insert(binding.id, storage);
                            // U4 — the dyn-Trait binder decided storage from the
                            // RHS SHAPE and minted from the shape alone. The
                            // warrant puts the RHS's provenance to the ledger and
                            // the authority, exactly as the plain `let` binder
                            // does; a proven-foreign initializer earns the fat
                            // pointer no vtable-slot-0 release.
                            let warrant =
                                self.owner_warrant_for_initializer(binding.id, value, &value_ty);
                            self.register_owned_local(
                                binding.id,
                                binding.name.clone(),
                                value_ty.clone(),
                                warrant,
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
                } else if let Some(warrant) = ordinary_owner_warrant {
                    // Only register the binding in `owned_locals` when
                    // the same iteration will also wire `binding_locals`
                    // (either pre-emptively via the lambda-actor
                    // `pending` path above, or via the `Some(src)` arm
                    // below). Keeping the two ledgers in sync is the
                    // structural invariant that owner publication
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
                    let inherited_alias = match &value.kind {
                        HirExprKind::BindingRef {
                            resolved: ResolvedRef::Binding(source),
                            ..
                        } => self.exact_owned_local_alias_provenance(*source, &binding_ty),
                        _ => super::ownership::OwnedAliasInheritance::NotAlias,
                    };
                    let alias_inheritance_ambiguous = matches!(
                        &inherited_alias,
                        super::ownership::OwnedAliasInheritance::Ambiguous
                    );
                    let inherited_alias_provenance = match inherited_alias {
                        super::ownership::OwnedAliasInheritance::Exact(provenance) => {
                            Some(provenance)
                        }
                        super::ownership::OwnedAliasInheritance::NotAlias => None,
                        super::ownership::OwnedAliasInheritance::Ambiguous => {
                            self.diagnostics.push(MirDiagnostic {
                                kind: MirDiagnosticKind::NotYetImplemented {
                                    construct: "ambiguous byte-copy alias rebind".to_string(),
                                    site: value.site,
                                },
                                note: "the initializer binding has multiple owner-ledger rows or a mismatched aggregate type; MIR refuses to mint a successor over storage whose alias provenance is not unique"
                                    .to_string(),
                            });
                            None
                        }
                    };
                    if !alias_inheritance_ambiguous {
                        match self
                            .field_projection_alias_provenance(value, &binding_ty)
                            .or(inherited_alias_provenance)
                        {
                            Some(provenance) => self.register_owned_local_alias(
                                binding.id,
                                binding.name.clone(),
                                binding_ty.clone(),
                                provenance,
                                warrant,
                            ),
                            None => self.register_owned_local(
                                binding.id,
                                binding.name.clone(),
                                binding_ty.clone(),
                                warrant,
                            ),
                        }
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
                    if is_for_await_handle_cursor {
                        if let Some(scope) = self.active_scopes.last().copied() {
                            self.scope_stream_bindings.push((
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
                            let slot = *self.binding_locals.get(&binding.id).expect(
                                "let binding storage must be established before owner mint",
                            );
                            // A `let` share of an ACTIVE `string` yield binder
                            // gets the same inline retain-backed share as the
                            // `assign` path: the binder's count stays with its
                            // body-end release authority, the new binding owns
                            // the explicit `+1`, and the drop-safety scan
                            // keeps the binder's per-iteration drop.
                            if matches!(binding_ty, ResolvedTy::String)
                                && matches!(value.kind, HirExprKind::BindingRef { .. })
                                && self.active_yield_binder_place(src)
                            {
                                self.push_instr(Instr::StringRetain {
                                    value: src,
                                    condition: crate::model::StringRetainCondition::FreshShare,
                                });
                                self.yield_share_instr_exempt
                                    .insert((self.current_block_id, self.instructions.len()));
                            }
                            self.push_instr(Instr::Move { dest: slot, src });
                            if borrowed_runtime_result {
                                self.push_instr(Instr::OwnershipEvent(
                                    crate::model::OwnershipEvent::AliasRelocate {
                                        from: src,
                                        to: slot,
                                    },
                                ));
                                self.borrowed_runtime_result_places.remove(&src);
                                self.borrowed_runtime_result_places.insert(slot);
                            }
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
                    self.retire_provisional_owner_for_bound_value(binding.id, &binding.name, src);
                }
                // Cursor ownership is sealed only after the physical binding
                // move. A whole-VecIter rebind can then publish one exact
                // source-generation -> destination-generation Transfer at
                // that program point; registering before the Move minted a
                // parallel owner and left both generations attached to the
                // destination slot.
                self.register_vec_iter_scope_owner(
                    binding.id,
                    &binding.name,
                    value,
                    &binding_ty,
                    pending || value_place.is_some(),
                    value_place,
                );
                // #1933 / #1941 — allocate the path-sensitive drop-flag for a
                // non-idempotent user `#[resource]` OWNER now that its backend
                // Place is wired into `binding_locals`. A typed runtime result
                // whose contract returned `InteriorAliasOfReceiver` is not an
                // owner: the collection retains the sole close authority and
                // the alias validator below diagnoses escape/consume/rebind.
                // Giving that binder an affine flag would fabricate cleanup
                // authority (and cannot publish a Guard because no generation
                // was minted). Keep missing-owner failures closed for every
                // other affine binder by retaining the assertion inside
                // `maybe_alloc_affine_release_flag`.
                if borrowed_runtime_result {
                    let place = self
                        .binding_locals
                        .get(&binding.id)
                        .copied()
                        .expect("typed interior-alias binding must have backend storage");
                    assert!(
                        self.current_owner_id_at_place(place).is_none(),
                        "typed interior-alias binding {:?} unexpectedly owns {:?}",
                        binding.id,
                        place
                    );
                } else {
                    // Zero-initialised here so the flag dominates every later
                    // `Consume` use site and every scope-exit drop; set to 1 at
                    // each consume. A no-op for every other binding class (see
                    // `affine_release_needs_drop_flag`).
                    self.maybe_alloc_affine_release_flag(binding.id, &binding_ty);
                }
                self.maybe_alloc_overwrite_guard_flag(binding);
                // #2418 — allocate the path-sensitive scope-exit drop-flag for
                // an owned collection local the pre-pass saw consumed, so a
                // conditional move keeps its (flag-gated) scope-exit release
                // on the not-moved path instead of retracting it entirely.
                self.maybe_alloc_collection_drop_flag(binding, &binding_ty);
                self.maybe_alloc_conditional_record_drop_flag(
                    binding,
                    &binding_ty,
                    owned_string_record_key.is_some(),
                );
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
                let returned_binding = match &expr.kind {
                    HirExprKind::BindingRef {
                        resolved: ResolvedRef::Binding(binding),
                        ..
                    } => Some(*binding),
                    _ => None,
                };
                let value_place = self.lower_value_for_move(expr);
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
                self.record_active_iteration_owner_drops_for_exit_edge(0);
                self.emit_stream_drops_for_exit_edge(0);
                // Release every `for x in …` snapshot cursor this return
                // abandons (`emit_vec_iter_drops_for_exit_edge`); the lexical
                // fall-through close is past the return and its `ScopeReleased`
                // disposition also empties the return plan, so without this the
                // cursor's whole snapshot tree leaks once per call.
                self.emit_vec_iter_drops_for_exit_edge_except(0, returned_binding);
                // Seal the current basic block with Terminator::Return so
                // codegen actually emits an early return at this program
                // point. Codegen consumes the block terminator (not the
                // `MirStatement::Return` statement marker), so without sealing
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
                self.record_active_iteration_owner_drops_for_exit_edge(0);
                self.emit_stream_drops_for_exit_edge(0);
                // Same cursor exit-edge release as Return(Some) above.
                self.emit_vec_iter_drops_for_exit_edge(0);
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
            if matches!(symbol.as_str(), "hew_observe_scrape" | "hew_observe_series")
                && matches!(self.subst_ty(&expr.ty), ResolvedTy::String)
            {
                // Keep a discarded transferred string on the value route used
                // by ordinary extern expressions. That materialises the final
                // unsafe-block-normalised local, which the shared audited
                // fresh-temp collector owns and releases. The bespoke observe
                // helper's old Discarded route had no such local to audit.
                let _ = self.lower_value(expr);
            } else {
                let _ = self.lower_runtime_call(
                    &symbol,
                    args,
                    site,
                    RuntimeCallContext::Discarded,
                    Some(&expr.ty),
                );
            }
        } else {
            // Discarded expression. Any fresh-owned `string` temporary the
            // expression produces — `xs[i]`/`xs.get(i)` over `Vec<string>`
            // (`hew_vec_get_str`), `a + b`, `.to_uppercase()`, … — is released
            // by the general owned-`string` temporary substrate's DISCARD path
            // (`apply_nested_fresh_string_temp_drops`), which splices one inline
            // `hew_string_drop` after the unused producer. No Vec-specific
            // handling is owed here.
            //
            if !self.typed_produced_value_demand_is_resolved(
                expr,
                "discarded result has unresolved ownership",
            ) {
                return;
            }
            let discarded_vec_iter_ty = self.subst_ty(&expr.ty);
            if self
                .vec_iter_cursor_release_symbol(&discarded_vec_iter_ty)
                .is_some()
            {
                if let Some(place) = self.lower_vec_iter_value_for_read(expr) {
                    if let Some(flag) = self.vec_iter_value_drop_flags.get(&expr.site).copied() {
                        let owner = self.vec_iter_value_owners.get(&expr.site).copied();
                        self.emit_flag_gated_vec_iter_value_release(
                            place,
                            &discarded_vec_iter_ty,
                            flag,
                            owner,
                        );
                    }
                }
            } else if let Some(place) = self.lower_value(expr) {
                if let Some(ty) = self.discarded_vec_iter_next_owned_ty(expr) {
                    self.push_instr(Instr::Drop {
                        place,
                        ty,
                        drop_fn: Some(crate::model::DropFnSpec::InPlace(
                            crate::ownership::InPlaceReleaseKind::Enum,
                        )),
                    });
                } else {
                    self.register_discarded_call_result_owner(expr, place);
                }
            }
        }
    }

    /// Lower an expression into the backend instruction stream and return the
    /// `Place` that holds the expression's value (or `None` if the construct is outside the
    /// spine subset — a `MirDiagnostic` is recorded in that case).
    #[allow(
        clippy::too_many_lines,
        reason = "single large match on HirExprKind variants; each arm is a fail-closed \
                  boundary rule and splitting would obscure the exhaustiveness requirement"
    )]
    /// The one publication boundary for expression results. Recursive lowering
    /// returns here before a parent advances to its next argument or field.
    pub(crate) fn lower_value(&mut self, expr: &HirExpr) -> Option<Place> {
        let value = self.lower_value_inner(expr);
        if let Some(place) = value {
            // Specialised HIR rewrites retain consumed checker children as
            // non-evaluated source anchors. Publish those source occurrences to
            // the specialised operation's one result place before adopting the
            // parent row, so relation/receiver edges can resolve without either
            // a self-edge or duplicate evaluation.
            match &expr.kind {
                HirExprKind::ActorAsk {
                    source_anchor: Some(anchor),
                    ..
                } => self.publish_produced_value_source_anchor(expr, anchor, place),
                HirExprKind::ConnAwaitRead { source_anchor, .. }
                | HirExprKind::ListenerAwaitAccept { source_anchor, .. } => {
                    self.publish_produced_value_source_anchor(expr, source_anchor, place);
                }
                HirExprKind::MachineFieldAccess {
                    source_anchor: Some(anchor),
                    ..
                } => {
                    if let Some(binding) = self.current_machine_self_binding {
                        if let Some(source_place) = self.binding_locals.get(&binding).copied() {
                            self.published_value_places
                                .insert(anchor.site, source_place);
                        }
                    }
                }
                HirExprKind::MachineEventFieldAccess {
                    source_anchor: Some(anchor),
                    ..
                } => {
                    if let Some(binding) = self.current_machine_event_binding {
                        if let Some(source_place) = self.binding_locals.get(&binding).copied() {
                            self.published_value_places
                                .insert(anchor.site, source_place);
                        }
                    }
                }
                HirExprKind::SubsumedValue {
                    source,
                    producer: hew_hir::HirProducedValueProducer::Block,
                } => self.publish_transparent_block_sources(source, place),
                _ => {}
            }
            self.publish_produced_value_place(expr, place);
            if !self
                .suppress_typed_produced_owner_sites
                .contains(&expr.site)
            {
                self.adopt_typed_produced_value_owner(expr, place);
            }
        }
        value
    }

    fn publish_transparent_block_sources(&mut self, source: &HirExpr, place: Place) {
        if let HirExprKind::SubsumedValue {
            source: inner,
            producer: hew_hir::HirProducedValueProducer::Block,
        } = &source.kind
        {
            self.publish_transparent_block_sources(inner, place);
        }
        self.publish_produced_value_place(source, place);
    }

    fn publish_produced_value_source_anchor(
        &mut self,
        specialised: &HirExpr,
        anchor: &hew_hir::HirProducedValueSourceAnchor,
        place: Place,
    ) {
        if self
            .param_ownership
            .produced_value_facts
            .get(&specialised.site)
            .is_some_and(|fact| {
                matches!(fact.relation, hew_hir::HirProducedValueRelation::Subsumes(source) if source == anchor.site)
            })
        {
            // Subsumed anchors are provenance-only structural occurrences.
            // Publishing them would fabricate an independently materialized
            // child generation over the parent's storage.
            return;
        }
        self.published_value_places.insert(anchor.site, place);
        let anchor_ty = self.subst_ty(&anchor.ty);
        let place_ty = match place {
            Place::Local(local) => self.locals.get(local as usize),
            _ => None,
        };
        // Source anchors preserve a consumed checker occurrence.  They are
        // executable ownership publications only when the specialised result
        // reuses type-congruent storage; a wrapper such as `R` ->
        // `Result<R, AskError>` is owned solely by the outer specialised node.
        if place_ty.is_some_and(|place_ty| place_ty != &anchor_ty) {
            return;
        }
        let mut source = specialised.clone();
        source.node = anchor.node;
        source.site = anchor.site;
        source.ty = anchor.ty.clone();
        source.value_class = anchor.value_class;
        source.intent = anchor.intent;
        source.span = anchor.span.clone();
        self.adopt_typed_produced_value_owner(&source, place);
        if let Some(nested) = &anchor.source {
            self.publish_produced_value_source_anchor(&source, nested, place);
        }
    }

    #[allow(
        clippy::too_many_lines,
        reason = "single exhaustive lowering dispatch keeps fail-closed HIR coverage visible"
    )]
    fn lower_value_inner(&mut self, expr: &HirExpr) -> Option<Place> {
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
                    let use_intent = self.binding_ref_use_intent(expr);
                    self.statements.push(MirStatement::Use {
                        binding: *id,
                        name: name.clone(),
                        site: expr.site,
                        ty: use_ty.clone(),
                        intent: use_intent,
                    });
                    let direct_vec_iter_move =
                        self.vec_iter_direct_move_sites.last().copied() == Some(expr.site);
                    if matches!(use_intent, IntentKind::Consume) || direct_vec_iter_move {
                        if let Some(flag) = self.vec_iter_drop_flags.get(id).copied() {
                            if direct_vec_iter_move {
                                if let Some(result_flag) =
                                    self.vec_iter_move_result_flags.last().copied()
                                {
                                    // Preserve the source's pre-transfer
                                    // ownership state for the destination
                                    // expression before neutralizing it.
                                    self.instructions.push(Instr::Move {
                                        dest: result_flag,
                                        src: flag,
                                    });
                                }
                            }
                            // A VecIter binding reached while producing a
                            // cursor for an owning sink transfers on this exact
                            // CFG path. The enclosing if/match arms may carry
                            // HIR Read intent, so the move-lowering context
                            // supplements (but does not alter) checker
                            // authority for those binding refs.
                            self.instructions.push(Instr::ConstI64 {
                                dest: flag,
                                value: 1,
                            });
                        }
                    }
                    if matches!(use_intent, IntentKind::Consume | IntentKind::Discharge)
                        && self.binding_seeds_drop_elaboration(&use_ty)
                    {
                        // #1933 / #1941 — a non-idempotent user `#[resource]`
                        // with an allocated path-sensitive drop-flag is KEPT in
                        // `owned_locals` so its owner's `Guard` narrows its
                        // close per control-flow path. Mark the flag
                        // consumed (set 1) so codegen's
                        // `flag == 0` gate skips the now-callee-owned close on
                        // this path; the dataflow's own `Use{Consume}` transition
                        // (independent of `owned_locals`) still drives the
                        // move-checker and the per-exit `BindingState`. Every
                        // other consumed owned class keeps the legacy
                        // path-insensitive `owned_locals` removal.
                        if use_intent == IntentKind::Discharge {
                            // A consuming method may unwind before it
                            // discharges the receiver. Keep both its OwnerId
                            // and guard live through the call; the finalized
                            // CFG publishes the flag store, terminal Transfer,
                            // and physical neutralization in the normal
                            // successor only.
                        } else if self.vec_iter_drop_flags.contains_key(id) {
                            // Updated above even when a value-producing
                            // if/match arm retained HIR Read intent.
                        } else if self.affine_release_flags.contains_key(id) {
                            if self.deferred_affine_call_consume_sites.contains(&expr.site) {
                                // The typed call contract adopts this affine
                                // argument only if the invoke returns normally.
                                // Keep its guard and OwnerId live in this block
                                // so unwind cleanup retains the caller's value;
                                // the finalized call-site fact commits both in
                                // the normal successor. The checker-visible
                                // `Use { Consume }` above remains unchanged.
                                self.set_owned_local_consumed_post_lowering(
                                    *id,
                                    None,
                                    super::DischargeSite::CallArgumentTransfer,
                                );
                            } else {
                                self.set_owned_local_consumed(
                                    *id,
                                    None,
                                    super::DischargeSite::BindingMoved,
                                );
                            }
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
                        } else if let Some(flag) =
                            self.actor_message_cow_drop_flags.get(id).copied()
                        {
                            // A mailbox-owned CoW leaf can be moved on one
                            // branch and remain handler-owned on another.
                            // Preserve it in the scope-exit ledger and record
                            // the path-local transfer for the guarded drop.
                            self.instructions.push(Instr::ConstI64 {
                                dest: flag,
                                value: 1,
                            });
                        } else if let Some(flag) =
                            self.conditional_record_drop_flags.get(id).copied()
                        {
                            // A fresh record may transfer only on this CFG
                            // edge. Keep its local drop registered for sibling
                            // paths and record the executed handoff here.
                            self.instructions.push(Instr::ConstI64 {
                                dest: flag,
                                value: 1,
                            });
                        } else if self.deferred_affine_call_consume_sites.contains(&expr.site) {
                            // A catalogued consuming runtime call adopts the
                            // value only on its normal edge. Keep the OwnerId
                            // live through the invoke (and through any bounds
                            // check lowered before it); the checker-visible
                            // `Use { Consume }` above stays, and
                            // `splice_normal_call_ownership_commits` publishes
                            // the one terminal Transfer in the normal successor.
                            self.set_owned_local_consumed_post_lowering(
                                *id,
                                None,
                                super::DischargeSite::CallArgumentTransfer,
                            );
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
                                    self.push_move_out_neutralize(
                                        provenance.source_place,
                                        crate::model::NeutralizeAuthority::MoveOutArmConsume,
                                    );
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
                                    let carrier_transfer =
                                        self.binding_locals.get(id).is_some_and(|place| {
                                            self.owned_carrier_authority(*place).is_some()
                                        });
                                    if !carrier_transfer {
                                        self.push_move_out_neutralize(
                                            provenance.source_place,
                                            crate::model::NeutralizeAuthority::EphemeralTempConsume,
                                        );
                                    }
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
                if let Some(dest) = self.lower_capture_env_binding_ref(*id, name, expr.site) {
                    return Some(dest);
                }
                let place = self.binding_locals.get(id).copied();
                if let Some(place) = place {
                    if matches!(
                        self.binding_ref_use_intent(expr),
                        IntentKind::Consume | IntentKind::Discharge
                    ) && matches!(
                        ValueClass::of_ty(&use_ty, &self.type_classes),
                        ValueClass::AffineResource | ValueClass::Linear
                    ) && matches!(
                        self.owned_carrier_authority(place),
                        Some(super::OwnedCarrierNeutralizeTarget::Whole(
                            Place::MachineVariant { .. } | Place::EnumVariant { .. }
                        ))
                    ) {
                        return Some(self.transfer_owned_carrier_place(place, &use_ty));
                    }
                }
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
                // A first-class string-returning named function crosses the
                // uniform ClosureInvoke ABI. Admit it only when the module's
                // string-carrier authority proves the target already returns
                // one independently releasable share. This keeps an
                // ownership-opaque extern (or a Hew wrapper around one) from
                // being laundered merely by assigning it to `fn() -> string`.
                // Ordinary closures are lowered through their own compiler
                // shim, whose parameter/capture/fresh-result return paths
                // establish this postcondition directly.
                if matches!(fn_ret_ty, ResolvedTy::String)
                    && !self
                        .call_scrutinee_provenance
                        .owned_string_return_carrier_symbols
                        .contains(&fn_symbol)
                {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::NotYetImplemented {
                            construct: format!(
                                "named function `{fn_symbol}` used as a first-class \
                                 string-returning value without an owned-return contract"
                            ),
                            site: expr.site,
                        },
                        note: "a `fn(...) -> string` callable must return one independently \
                               releasable string share; this target is ownership-opaque, so \
                               routing it through ClosureInvoke would manufacture a caller \
                               drop obligation"
                            .to_string(),
                    });
                    return None;
                }
                let shim_name = format!(
                    "__hew_named_fn_invoke_{}",
                    Self::sanitize_symbol_component(&fn_symbol)
                );
                // Emit the shim only once per named fn (dedup by shim_name).
                //
                // APPROXIMATION — this shim is deduped MODULE-wide by
                // `shim_name` (here, and again by `HashSet<String>` in
                // `flatten_generated_functions`, `lower/mod.rs`), but the
                // `MirCallableKey` minted below is parented on `self`'s own
                // key — i.e. the body that happened to reference `fn_symbol`
                // as a value first — not on the named function it wraps. WHY:
                // one shim body per named function is what codegen wants (a
                // second identical shim would be a duplicate LLVM symbol),
                // and the parent is only ever read as part of a unique
                // identity, never as "the body that owns this shim".
                // Encounter order is deterministic for a fixed input, so the
                // key is stable — `compile-determinism-verify` covers that.
                // WHEN this stops being good enough: as soon as a consumer
                // treats the parent as ownership (e.g. attributing the
                // shim's drops or diagnostics to the parent body), because
                // two referencing bodies would then disagree about which of
                // them owns it. WHAT replaces it: parent the shim on the
                // WRAPPED named function's `MirCallableKey` — it is a
                // per-callee artifact, not a per-caller one — which needs
                // that key in hand here rather than only `fn_symbol`. Same
                // approximation as `ensure_task_entry_adapter`
                // (`lower/task.rs`); tracked in `.tmp/TODO.md`.
                if !self
                    .generated_functions
                    .iter()
                    .any(|f| f.raw.name == shim_name)
                {
                    let shim_key = self.mint_synthesized_child_key(
                        crate::model::SynthesizedCallable::NamedFnInvokeShim,
                    );
                    let shim = self.lower_named_fn_invoke_shim(
                        &fn_symbol, &shim_name, &shim_key, &param_tys, &fn_ret_ty,
                    );
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
                    // No environment record exists, so there is nothing to own.
                    env_ownership: Vec::new(),
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
                    .map(|elem| self.lower_value_for_move(elem))
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
            HirExprKind::RcIntrinsic {
                op,
                payload_ty,
                receiver,
                value,
                result_ty,
            } => Some(self.lower_rc_intrinsic(
                *op,
                payload_ty,
                receiver.as_deref(),
                value.as_deref(),
                result_ty,
            )),
            HirExprKind::GeneratorNext { receiver, yield_ty } => {
                let ctx = self.lower_value(receiver)?;
                let ctx_owner = match &receiver.kind {
                    HirExprKind::BindingRef {
                        resolved: ResolvedRef::Binding(binding),
                        ..
                    } => self
                        .owner_generations
                        .get(binding)
                        .copied()
                        .map(|generation| crate::model::OwnerId {
                            binding: *binding,
                            generation,
                        }),
                    _ => None,
                };
                // `expr.ty` is the checker-authoritative `Option<yield_ty>`; the
                // dest enum slot is allocated with that exact type so codegen
                // resolves the registered Option layout for the unbox.
                let dest = self.alloc_local(expr.ty.clone());
                self.push_instr(Instr::GeneratorNext {
                    dest,
                    ctx,
                    ctx_owner,
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
            HirExprKind::Call {
                target,
                callee,
                args,
            } => {
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
                        && callee.ty.is_builtin(BuiltinType::LambdaPid)
                    {
                        return self.lower_lambda_actor_call(callee, args, &expr.ty, expr.site);
                    }
                }
                // Direct-call target authority.  A `BindingRef` remains useful
                // for source locations and ItemId-backed ownership facts, but
                // never selects the callee.  In particular, a user declaration
                // called `log` must not become the runtime math builtin `log`
                // merely because their leaf spellings collide.
                let callee_item = match &callee.kind {
                    HirExprKind::BindingRef {
                        resolved: ResolvedRef::Item(item),
                        ..
                    } => Some(*item),
                    _ => None,
                };
                match target {
                    hew_types::CallTarget::User(declaration) => {
                        // The declaration ID is the semantic source and its
                        // exact HIR declaration-to-symbol projection is the
                        // linker authority.  No dotted-name reconstruction or
                        // leaf-name retry is permitted below: an absent map
                        // entry is a broken HIR/MIR boundary, not a spelling
                        // the backend may recover.
                        let Some(symbol) = self.direct_call_symbols.get(declaration).cloned()
                        else {
                            self.diagnostics.push(MirDiagnostic {
                                kind: MirDiagnosticKind::NotYetImplemented {
                                    construct: format!(
                                        "direct declaration `{}` without an HIR symbol map",
                                        declaration.full_path()
                                    ),
                                    site: expr.site,
                                },
                                note: "MIR refuses to reconstruct a user-function linker label from a declaration path"
                                    .to_string(),
                            });
                            return None;
                        };
                        let symbol = self.project_direct_call_symbol(symbol, expr.site);
                        if !self.module_fn_names.contains(&symbol) {
                            self.diagnostics.push(MirDiagnostic {
                                kind: MirDiagnosticKind::NotYetImplemented {
                                    construct: format!(
                                        "direct declaration `{}` has no emitted MIR body",
                                        declaration.full_path()
                                    ),
                                    site: expr.site,
                                },
                                note: "the checker-selected declaration ID did not project to an emitted symbol; MIR will not retry the callee spelling"
                                    .to_string(),
                            });
                            return None;
                        }
                        return self.lower_direct_call(
                            &symbol,
                            None,
                            callee_item,
                            args,
                            &expr.ty,
                            expr.site,
                        );
                    }
                    hew_types::CallTarget::Runtime(family) => {
                        if family == &hew_types::runtime_call::RuntimeCallFamily::StringConcat {
                            return self.lower_string_concat_runtime_call(
                                args,
                                expr.site,
                                RuntimeCallContext::ValueNeeded,
                                Some(&expr.ty),
                            );
                        }
                        if family == &hew_types::runtime_call::RuntimeCallFamily::StructuralFormat {
                            if args.len() != 1 {
                                self.diagnostics.push(MirDiagnostic {
                                    kind: MirDiagnosticKind::NotYetImplemented {
                                        construct: "structural format arity".to_string(),
                                        site: expr.site,
                                    },
                                    note: format!(
                                        "structural formatting expects one argument, got {}",
                                        args.len()
                                    ),
                                });
                                return None;
                            }
                            let value = self.lower_value(&args[0])?;
                            let dest = self.alloc_local(ResolvedTy::String);
                            self.push_runtime_call(
                                "hew_structural_format",
                                vec![value],
                                Some(dest),
                            );
                            return Some(dest);
                        }
                        // `runtime_symbol_for_call_expr` handled the ABI subset
                        // above.  The remaining typed families are the explicit
                        // direct/codegen-intercept partition.
                        let symbol = family.c_symbol();
                        if !self.module_fn_names.contains(symbol) {
                            self.diagnostics.push(MirDiagnostic {
                                kind: MirDiagnosticKind::NotYetImplemented {
                                    construct: format!(
                                        "runtime family `{family:?}` has no direct MIR route"
                                    ),
                                    site: expr.site,
                                },
                                note: "MIR consumes the checker-selected runtime family and does not retry a callee name"
                                    .to_string(),
                            });
                            return None;
                        }
                        return self.lower_direct_call(
                            symbol,
                            Some(*family),
                            None,
                            args,
                            &expr.ty,
                            expr.site,
                        );
                    }
                    hew_types::CallTarget::Builtin { endpoint } => {
                        // Catalog builtin endpoints are checker/HIR-validated
                        // call identities.  Unlike `Runtime`, some use a
                        // codegen-only linkage such as `PrintIntercept` and
                        // therefore have no RuntimeCallFamily.  The endpoint
                        // itself is the closed catalog key; MIR does not
                        // reinterpret the source callee spelling.
                        // A catalog FFI shim has two distinct identities: the
                        // checker-selected catalog endpoint (for example
                        // `len_str`) and the concrete runtime symbol its ABI
                        // declaration/codegen entry uses
                        // (`hew_string_length`).  Join them by the callee's
                        // catalog ItemId, never by a spelling lookup: a user
                        // `extern` is allowed to share the endpoint spelling,
                        // but must not inherit the catalog row's runtime
                        // ownership facts.  Keeping the emitted symbol here
                        // makes the raw-MIR call, representation-effect facts,
                        // and codegen all refer to the same concrete ABI edge.
                        let callee_symbol = callee_item
                            .and_then(|item| {
                                crate::return_provenance::stdlib_shim_emitted_symbol(endpoint, item)
                            })
                            .unwrap_or(endpoint.as_str());
                        if !self.module_fn_names.contains(callee_symbol) {
                            self.diagnostics.push(MirDiagnostic {
                                kind: MirDiagnosticKind::NotYetImplemented {
                                    construct: format!(
                                        "catalog builtin endpoint `{endpoint}` (emitted as \
                                         `{callee_symbol}`) has no direct MIR route"
                                    ),
                                    site: expr.site,
                                },
                                note: "MIR consumes the HIR-selected catalog endpoint and does not retry a callee name"
                                    .to_string(),
                            });
                            return None;
                        }
                        // Catalog linkage is the authority for a compiled FFI
                        // boundary.  Some concrete ABI shims intentionally sit
                        // outside `RuntimeCallFamily` (for example
                        // `hew_string_length`), so treating a missing runtime
                        // family as an ordinary direct call loses the checked
                        // borrowing contract in imported bodies.  Join the
                        // endpoint to its catalog linkage and require the
                        // ItemId-projected symbol to agree; this never grants
                        // `Extern` authority based on a user-controlled symbol
                        // spelling.
                        let authority = compiler_builtin_call_authority(endpoint)
                            .or_else(|| {
                                hew_types::runtime_call::RuntimeCallFamily::from_c_symbol(
                                    callee_symbol,
                                )
                                .map(crate::CallAuthority::Runtime)
                            })
                            .or_else(|| {
                                hew_hir::stdlib_catalog::trusted_ffi_symbol_for_endpoint(endpoint)
                                    .filter(|symbol| *symbol == callee_symbol)
                                    .map(|_| crate::CallAuthority::Extern)
                            })
                            .or_else(|| {
                                // No ItemId projection fired, so the emitted
                                // callee spelling IS the closed catalog
                                // endpoint the checker selected — there is no
                                // user-controlled spelling in play at all, and
                                // the equality filter above has nothing to
                                // compare.  Grant the ownership-contract
                                // capability whenever that endpoint's catalog
                                // row crosses an audited runtime ABI.
                                //
                                // Two row families reach MIR this way and both
                                // previously fell to `Direct`: the print
                                // family, whose `PrintIntercept` linkage names
                                // `hew_print_value` rather than the callee
                                // spelling, and `string_concat`, whose surface
                                // name is not its `hew_string_concat` ABI
                                // symbol.  `Direct` means "opaque callee, no
                                // readable contract", which poisons every
                                // caller-visible-projection parameter passed to
                                // it into `RejectUnprovenRepresentationMutation`
                                // — so `fn f(k: string) { println(k) }` was
                                // rejected at direct-call codegen even though
                                // `hew_print_value`'s audited contract borrows
                                // its operand.
                                (callee_symbol == endpoint.as_str()
                                    && hew_hir::stdlib_catalog::endpoint_crosses_audited_runtime_abi(
                                        endpoint,
                                    ))
                                .then_some(crate::CallAuthority::Extern)
                            })
                            .unwrap_or_default();
                        return self.lower_direct_call_with_authority(
                            callee_symbol,
                            callee_item,
                            args,
                            &expr.ty,
                            expr.site,
                            authority,
                        );
                    }
                    hew_types::CallTarget::ImplMethod(declaration) => {
                        if let Some(callee) = stdlib_string_display_impl_callee(declaration) {
                            return self.lower_direct_call_with_authority(
                                callee,
                                None,
                                args,
                                &expr.ty,
                                expr.site,
                                catalog_display_call_authority(callee),
                            );
                        }
                        let Some(symbol) = self.direct_call_symbols.get(declaration).cloned()
                        else {
                            self.diagnostics.push(MirDiagnostic {
                                kind: MirDiagnosticKind::NotYetImplemented {
                                    construct: format!(
                                        "direct impl method `{}` without an HIR symbol map",
                                        declaration.full_path()
                                    ),
                                    site: expr.site,
                                },
                                note: "MIR refuses to reconstruct an impl-method linker label from a leaf method name"
                                    .to_string(),
                            });
                            return None;
                        };
                        let symbol = self.project_direct_call_symbol(symbol, expr.site);
                        if !self.module_fn_names.contains(&symbol) {
                            self.diagnostics.push(MirDiagnostic {
                                kind: MirDiagnosticKind::NotYetImplemented {
                                    construct: format!(
                                        "impl method `{}` has no emitted MIR body",
                                        declaration.full_path()
                                    ),
                                    site: expr.site,
                                },
                                note: "the exact HIR declaration-to-symbol projection did not name an emitted function"
                                    .to_string(),
                            });
                            return None;
                        }
                        return self.lower_direct_call(
                            &symbol,
                            None,
                            callee_item,
                            args,
                            &expr.ty,
                            expr.site,
                        );
                    }
                    hew_types::CallTarget::Extern {
                        declaration: _,
                        endpoint,
                        trusted_compiled_stdlib,
                    } => {
                        // The endpoint was validated and attached by the
                        // checker-side extern-symbol rewrite.  It is not a
                        // source declaration lookup and therefore must not be
                        // routed through the impl-body symbol map.
                        let authority = if *trusted_compiled_stdlib {
                            crate::CallAuthority::Extern
                        } else {
                            crate::CallAuthority::Direct
                        };
                        return self.lower_direct_call_with_authority(
                            endpoint, None, args, &expr.ty, expr.site, authority,
                        );
                    }
                    hew_types::CallTarget::Unsupported { reason } => {
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::UnsupportedNode {
                                reason: format!("unsupported checker call target: {reason}"),
                            },
                            note: "HIR must not pass an unsupported call target to MIR".to_string(),
                        });
                        return None;
                    }
                    hew_types::CallTarget::RuntimeCollection(_)
                    | hew_types::CallTarget::DynamicVtable { .. }
                    | hew_types::CallTarget::StaticTraitMethod { .. } => {
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::UnsupportedNode {
                                reason: "non-ordinary CallTarget reached ordinary call lowering"
                                    .to_string(),
                            },
                            note: "this target family must use its dedicated HIR call variant"
                                .to_string(),
                        });
                        return None;
                    }
                    hew_types::CallTarget::IndirectFunctionValue => {}
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
                    let mut vec_iter_read_args = Vec::new();
                    for arg in args {
                        let arg_ty = self.subst_ty(&arg.ty);
                        if self.vec_iter_cursor_release_symbol(&arg_ty).is_some() {
                            let place = self.lower_vec_iter_value_for_read(arg)?;
                            if let Some(flag) =
                                self.vec_iter_value_drop_flags.get(&arg.site).copied()
                            {
                                let owner = self.vec_iter_value_owners.get(&arg.site).copied();
                                vec_iter_read_args.push((place, arg_ty, flag, owner));
                            }
                            arg_places.push(place);
                        } else {
                            // An indirect call is a call: it BORROWS every
                            // non-resource argument, exactly like the direct
                            // path (`lower_direct_call_args`). A closure's
                            // parameter list carries no `consume` marker and
                            // no callee item to consult, so there is no
                            // argument here a call may take. Routing these
                            // through the move funnel neutralized any carrier
                            // authority the argument held — a summary-owned
                            // parameter handed to a borrowing closure lost the
                            // release its own callee still owed, and the next
                            // owning use of that slot faulted.
                            arg_places.push(self.lower_method_arg_value(arg, false)?);
                        }
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
                        for (place, ty, flag, owner) in vec_iter_read_args {
                            self.emit_flag_gated_vec_iter_value_release(place, &ty, flag, owner);
                        }
                        return dest;
                    }
                    self.push_instr(Instr::CallClosure {
                        call_site: expr.site,
                        callee: callee_place,
                        args: arg_places,
                        ret_ty,
                        dest,
                    });
                    for (place, ty, flag, owner) in vec_iter_read_args {
                        self.emit_flag_gated_vec_iter_value_release(place, &ty, flag, owner);
                    }
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
                    if let Some(src) = self.lower_composite_result_value(tail) {
                        let secured = self.alloc_local(self.subst_ty(&tail.ty));
                        self.push_composite_result_move(secured, src, &tail.ty);
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
                self.emit_scope_exit_marker_with_carries([block.scope], result.iter().copied());
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
                let record_key = user_record_layout_key(&expr_ty).unwrap_or_else(|| name.clone());
                let is_vec_iter_cursor = matches!(
                    &expr_ty,
                    ResolvedTy::Named {
                        args,
                        builtin: Some(BuiltinType::VecIter),
                        ..
                    } if args.len() == 1
                );
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
                    // The Vec for-in desugar marks only the borrowed source of
                    // `VecIter { vec, idx }` as Capture. That field aliases a
                    // still-owning source binding, while the cursor ownership
                    // bit starts moved. Keep this one record ingress out of the
                    // owned-carrier transfer funnel: moving and neutralizing
                    // the source here would disable both release authorities.
                    //
                    // Fence the exception by record and field instead of
                    // treating every CowValue Capture as non-transferring;
                    // closure-environment and other aggregate ingress sites
                    // retain their ordinary ownership-boundary semantics.
                    let borrows_vec_iter_source = is_vec_iter_cursor
                        && fname == "vec"
                        && fexpr.intent == hew_hir::IntentKind::Capture;
                    // A consuming cursor over a BARE actor state field
                    // (`plugins.into_iter()`) must neither alias-share the
                    // handle (the cursor's own free would dangle the
                    // persistent slot: next dispatch reads freed heap,
                    // teardown double-frees — dogfood F3) nor deep-clone it
                    // (drop-only dyn elements have no clone thunk). Route the
                    // loaded handle through `hew_vec_take_all`: the cursor
                    // owns the moved buffer, the state slot stays a valid
                    // empty vec with its stamped element descriptor — later
                    // pushes and the exactly-once state drop stay sound.
                    let takes_persistent_state_vec = is_vec_iter_cursor
                        && fname == "vec"
                        && self.vec_field_src_consumes_bare_actor_state_field(fexpr);
                    let place = if borrows_vec_iter_source {
                        self.lower_value(fexpr)
                    } else if takes_persistent_state_vec {
                        self.lower_vec_take_all_from_state_field(fexpr)
                    } else {
                        self.lower_value_for_move(fexpr)
                    };
                    if let Some(place) = place {
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
                    let place = self.lower_value_for_move(base_expr);
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
                // the OLD base value at the construction site via
                // `RecordFieldDrop`. Single-pointer leaves release their live
                // slot directly; `bytes` reaches field 0 of its fat
                // `{ ptr, offset, len }` slot for both release and poison.
                // Without this release the overwritten allocation is orphaned —
                // the functional-update overridden-owned-field LEAK (the bug the
                // leak oracle pins). Owned-aggregate overrides (record / tuple /
                // enum) remain a follow-on guarded by the fail-closed pre-flight
                // below.
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
                // Double-drop avoidance: the base is consume-marked above, so
                // its owner is discharged at this site and no exit plan
                // carries a composite drop for it; the overridden fields are
                // released here and the non-overridden fields move into the
                // new record via RecordInit. No owned field of `base` is ever
                // dropped twice.
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
                // `RecordFieldLoad`. `carry_transfers_field_ownership` is the
                // single authority for which field types that shallow read can
                // carry; it applies ONE rule at every nesting depth, so tuples
                // and records admit exactly the nested field types the same
                // field would admit bare. Fail closed with an NYI diagnostic
                // mirroring the override pre-flight rather than emit a double-free
                // or a silent leak. Lifting a specific type's carry is tracked in
                // hew-lang/hew#2207 (closure/`fn` env carry needs the env
                // retain/release spine that clone also lacks).
                if base_place.is_some() {
                    for (fname, fty) in &field_order {
                        if explicit.contains_key(fname.as_str()) {
                            continue; // Overridden — handled by the override path.
                        }
                        let subst_fty = self.subst_ty(fty);
                        if self.carry_transfers_field_ownership(&subst_fty) {
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
                                 env, an `@resource` / cancellation-token / task handle, an \
                                 `Option` or enum value, or a tuple or record containing one. \
                                 A tuple or record is carried only when EVERY nested field could \
                                 be carried in its own right, so wrapping an unsupported type \
                                 in another aggregate does not admit it. A non-heap tuple such as \
                                 `(i64, i64)` is also still rejected — conservatively, not \
                                 because carrying it is unsound. \
                                 The `..base` consumes the base, so carrying this field would \
                                 either release it twice (a double-free) or leave nothing \
                                 owning it (a leak). Set `{fname}` explicitly to a fresh value \
                                 in the update instead of carrying it through `..base`, or \
                                 clone the base into a fresh owned value first.",
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
                                // SINGLE MECHANISM for COW fields
                                // (`string` / `Vec<T>` / `HashMap` / `HashSet` /
                                // `Generator`, plus `bytes`):
                                // `RecordFieldDrop` (raw owning-word load →
                                // release → null-store). For `bytes`, codegen
                                // reaches the data-pointer word at field 0 of the
                                // `{ ptr, offset, len }` triple. It is the
                                // purpose-built op for an in-place field
                                // destructor and gives three things the old
                                // `RecordFieldLoad` + `Drop` split did not:
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
                // Field loads consume the same typed key as StructInit and
                // field stores. In particular, `VecIter<T>` / `HashMapIter<K,V>`
                // must retain their synthetic-record class; rebuilding a plain
                // name mangle here would miss the published layout and damage
                // the rest of the function's control-flow graph.
                let object_ty = self.subst_ty(&object.ty);
                let type_name = match &object_ty {
                    ResolvedTy::Named { name, .. } => {
                        user_record_layout_key(&object_ty).unwrap_or_else(|| name.clone())
                    }
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
                                // `construct` carries only the construct; the
                                // CLI diagnostic frame wraps it in the
                                // "MIR lowering for … is not implemented yet"
                                // sentence.
                                construct: format!(
                                    "field access on unregistered record type `{type_name}`"
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
                self.finalize_vec_clone_projection_base_owner(object, record_place);
                let dest = self.alloc_local(self.subst_ty(&expr.ty));
                self.push_instr(Instr::RecordFieldLoad {
                    record: record_place,
                    field_offset,
                    dest,
                });
                let field_ty = self.subst_ty(&expr.ty);
                self.note_carrier_projection(
                    record_place,
                    field_offset.0,
                    dest,
                    &field_ty,
                    expr.site,
                );
                self.publish_handle_transfer_projection(expr, &field_ty);
                Some(dest)
            }
            HirExprKind::Scope { body } => Some(self.lower_task_scope(body)),
            HirExprKind::SpawnedCall {
                callee,
                args,
                task_ty,
                bound,
                source_anchor: _,
            } => self.lower_spawned_call_task(callee, args, task_ty, *bound, expr.site),
            HirExprKind::ForkBlock { body, captures, .. } => {
                self.lower_fork_block_task(body, captures, expr.site)
            }
            HirExprKind::ScopeDeadline { duration, body } => {
                self.lower_scope_deadline(duration, body, expr.site)
            }
            HirExprKind::AwaitTask {
                binding_name,
                binding_id,
                output_ty,
                ..
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
                Some(self.emit_actor_self_handle_typed(&expr.ty))
            }
            HirExprKind::ActorSend {
                receiver,
                method_id,
                args,
                checked,
                blocking,
            } => self.lower_actor_send(receiver, method_id, args, *checked, *blocking, expr),
            HirExprKind::ActorAsk {
                receiver,
                method_id,
                args,
                reply_ty,
                deadline_ns,
                ..
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
                ..
            } => self.lower_conn_await_read(conn, *to_string, *deadline_ns, expr),
            HirExprKind::ListenerAwaitAccept {
                listener,
                deadline_ns,
                ..
            } => self.lower_listener_await_accept(listener, *deadline_ns, expr),
            HirExprKind::ChannelRecvAwait {
                receiver,
                deadline_ns,
                ..
            } => self.lower_channel_recv_await(receiver, *deadline_ns, expr),
            HirExprKind::StreamRecvAwait {
                stream,
                deadline_ns,
                ..
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
                let field_ty = self.subst_ty(&expr.ty);
                self.note_carrier_projection(inner_place, field_index, dest, &field_ty, expr.site);
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
                // Boxing a concrete value into an owned trait object moves the
                // concrete payload into the box. Publish that exact physical
                // handoff in MIR so the concrete source generation ends here
                // while the separately-minted dyn generation owns `dest`.
                // Anonymous aggregate producers are registered owners too;
                // relying only on `dyn_rebind_source_binding` misses them and
                // lets return/unwind cleanup destroy the boxed payload twice.
                self.push_instr(Instr::NeutralizePayloadSlot {
                    place: value_place,
                    transferee: Some(dest),
                    authority: crate::model::NeutralizeAuthority::WholeCarrierConsume,
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
                ..
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
                            "dyn-trait method call `{trait_name}.{method_name}` reached MIR with \
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
                // `hew_vec_push_layout` or `hew_vec_push_ptr` whose receiver Vec
                // has an owned (heap-owning) element must route to an owned push
                // so the ingress agrees with the owned constructor descriptor.
                // Otherwise a pointer/layout push byte-copies an affine handle
                // into the Vec while retaining the source close, which W3.053
                // correctly refuses. This upgrade is the array-literal-desugar
                // path's owned-ness decision: the HIR desugar bakes the plain
                // ABI from marker-only `ValueClass`, which cannot see structural
                // heap-ownership; MIR owns that authority. A genuine
                // checker-resolved owned `.push()` already carries
                // `hew_vec_push_owned`, and a real BitCopy element returns false
                // here, so this only ever corrects the synthesized guess — it
                // never re-derives the checker's impl-resolution verdict
                // (`dedup-semantic-boundary`).
                //
                // The owned-rewrite predicate is *family-gated* (must be a Vec
                // push) AND *symbol-keyed* (must be a plain `_layout` or `_ptr`
                // variant the HIR desugar emits). The family gate ensures we never
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
                                    "`Vec.{method_name}` on the type-parameter \
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
                ) && matches!(
                    target_symbol.as_str(),
                    "hew_vec_push_layout" | "hew_vec_push_ptr"
                ) && self.vec_receiver_has_owned_element(&receiver.ty)
                {
                    // Array literals are HIR-desugared to pushes into a synthetic
                    // Vec receiver. Their owned elements enter via the MOVE-in
                    // ABI: a fresh rvalue has no source drop to balance a
                    // clone-in, while a direct binding is consumed below so the
                    // descriptor slot is its sole owner. A user-authored
                    // `v.push(existing_owned)` keeps COPY-IN semantics (the
                    // source binding lives on and retains its own drop).
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
                    && (self.vec_receiver_has_drop_only_element(&receiver.ty)
                        || self.expr_is_owned_vec_move_ingress_owner(&args[0]))
                {
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
                    && (self.vec_receiver_has_drop_only_element(&receiver.ty)
                        || Self::expr_is_materialized_owner(
                            &args[1],
                            &self.call_scrutinee_provenance.fresh_owner_verdicts,
                            &self.funcupdate_param_ids,
                            &self.proven_foreign_bindings,
                        )) {
                    "hew_vec_set_owned_move".to_string()
                } else {
                    callee
                };

                // Receiver slots intentionally have no descriptor clone thunk.
                // A non-synthetic `push`/`set` that remains on the COPY-IN ABI
                // would reach `hew_vec_push/set_owned` and ask the runtime to
                // clone that endpoint. Array-literal and fresh-rvalue MOVE-in
                // have already been rewritten to their `_move` siblings above,
                // so rejecting only these exact symbols preserves the sole
                // supported construction path without weakening Sender.
                if self.vec_receiver_has_drop_only_element(&receiver.ty)
                    && matches!(callee.as_str(), "hew_vec_push_owned" | "hew_vec_set_owned")
                {
                    return self.reject_drop_only_vec_operation("push/set copy-in", expr.site);
                }

                // `hew_vec_get_clone` is the clone-out choke used by both
                // ordinary `Vec::get` and `VecIter::next`.  Concrete sites were
                // already admitted by the checker.  Generic sites reach this
                // arm with an abstract HIR type argument and are re-lowered
                // once per monomorphisation, so enforce the deferred
                // clone-totality obligation against the substituted element
                // before emitting the runtime call.  This preserves supported
                // `T = i64/string/record/...` instantiations while functions,
                // resources, opaque handles, and unresolved layouts fail
                // closed instead of receiving a shallow clone.
                if callee == "hew_vec_get_clone" {
                    if self.vec_receiver_has_drop_only_element(&receiver.ty) {
                        return self.reject_drop_only_vec_operation("get", expr.site);
                    }
                    let concrete_receiver = self.subst_ty(&receiver.ty);
                    let ResolvedTy::Named {
                        args: receiver_args,
                        builtin: Some(hew_types::BuiltinType::Vec),
                        ..
                    } = &concrete_receiver
                    else {
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct: "`VecIter` clone-out on a non-Vec receiver".to_string(),
                                site: expr.site,
                            },
                            note: format!(
                                "`hew_vec_get_clone` requires a concrete `Vec<E>` receiver, \
                                 but MIR substitution produced `{concrete_receiver}`"
                            ),
                        });
                        return None;
                    };
                    let Some(elem_ty) = receiver_args.first() else {
                        unreachable!("a resolved Vec receiver always carries one element argument");
                    };
                    if let Err(reason) = self.validate_collection_clone_value(elem_ty) {
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct: format!(
                                    "`VecIter<{}>` clone-out",
                                    elem_ty.user_facing()
                                ),
                                site: expr.site,
                            },
                            note: format!(
                                "`VecIter.next()` must clone each element into an independent \
                                 owner, but {reason}; the concrete generic instantiation is \
                                 rejected before the runtime clone choke"
                            ),
                        });
                        return None;
                    }
                }

                // A whole-Vec clone has the same semantic precondition as an
                // element clone-out: every stored element must have a real
                // clone/retain operation. `Vec::iter()` synthesizes this call
                // for a place receiver, so guarding only explicit `.clone()`
                // leaves an affine cursor snapshot reachable even when no
                // `next()` call is emitted.
                if matches!(
                    callee.as_str(),
                    "hew_vec_clone" | "hew_vec_clone_layout" | "hew_vec_clone_owned"
                ) {
                    if self.vec_receiver_has_drop_only_element(&receiver.ty) {
                        return self.reject_drop_only_vec_operation("clone/iter", expr.site);
                    }
                    let concrete_receiver = self.subst_ty(&receiver.ty);
                    let ResolvedTy::Named {
                        args: receiver_args,
                        builtin: Some(hew_types::BuiltinType::Vec),
                        ..
                    } = &concrete_receiver
                    else {
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct: "Vec clone on a non-Vec receiver".to_string(),
                                site: expr.site,
                            },
                            note: format!(
                                "`{callee}` requires a concrete `Vec<E>` receiver, but MIR \
                                 substitution produced `{concrete_receiver}`"
                            ),
                        });
                        return None;
                    };
                    let Some(elem_ty) = receiver_args.first() else {
                        unreachable!("a resolved Vec receiver always carries one element argument");
                    };
                    if let Err(reason) = self.validate_collection_clone_value(elem_ty) {
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct: format!("`Vec<{}>` clone", elem_ty.user_facing()),
                                site: expr.site,
                            },
                            note: format!(
                                "`Vec.clone()` / `Vec.iter()` must duplicate every element \
                                 into an independent owner, but {reason}; the clone is rejected \
                                 before it reaches the runtime"
                            ),
                        });
                        return None;
                    }
                }

                // HashMap whole-clone and snapshot projections manufacture
                // independent owners through descriptor clone functions.
                // Generic bodies are checked while K/V are abstract, so repeat
                // the clone-totality proof after monomorphisation, when the
                // receiver carries concrete K/V.
                //
                // Whole-map clone duplicates BOTH keys and values. `get` and
                // `values` duplicate V; `keys` duplicates K; `entries`
                // duplicates BOTH into the tuple elements. HashMap for-in
                // synthesizes `keys` + `values`, so it shares these same guards.
                let hashmap_clone_roles = match callee.as_str() {
                    "hew_hashmap_clone_layout" | "hew_hashmap_entries_layout" => Some((true, true)),
                    "hew_hashmap_get_layout"
                    | "hew_hashmap_get_clone_layout"
                    | "hew_hashmap_values_layout" => Some((false, true)),
                    "hew_hashmap_keys_layout" => Some((true, false)),
                    _ => None,
                };
                if let Some((clones_key, clones_value)) = hashmap_clone_roles {
                    let concrete_receiver = self.subst_ty(&receiver.ty);
                    let ResolvedTy::Named {
                        args: receiver_args,
                        builtin: Some(hew_types::BuiltinType::HashMap),
                        ..
                    } = &concrete_receiver
                    else {
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct: "HashMap clone on a non-HashMap receiver".to_string(),
                                site: expr.site,
                            },
                            note: format!(
                                "`{callee}` requires a concrete `HashMap<K, V>` receiver, but \
                                 MIR substitution produced `{concrete_receiver}`"
                            ),
                        });
                        return None;
                    };
                    let [key_ty, val_ty] = receiver_args.as_slice() else {
                        unreachable!(
                            "a resolved HashMap receiver always carries key and value arguments"
                        );
                    };
                    let mut clone_parts = Vec::with_capacity(2);
                    if clones_key {
                        clone_parts.push(("key", key_ty));
                    }
                    if clones_value {
                        clone_parts.push(("value", val_ty));
                    }
                    for (role, part_ty) in clone_parts {
                        if let Err(reason) = self.validate_collection_clone_value(part_ty) {
                            let operation = match callee.as_str() {
                                "hew_hashmap_clone_layout" => {
                                    "HashMap.clone() must duplicate every key and value"
                                }
                                "hew_hashmap_keys_layout" => {
                                    "HashMap.keys() must clone every key into an independent snapshot"
                                }
                                "hew_hashmap_values_layout" => {
                                    "HashMap.values() must clone every value into an independent snapshot"
                                }
                                "hew_hashmap_entries_layout" => {
                                    "HashMap.entries() must clone every key and value into an independent snapshot"
                                }
                                _ => {
                                    "HashMap.get() must clone the matched value into an independent owner"
                                }
                            };
                            self.diagnostics.push(MirDiagnostic {
                                kind: MirDiagnosticKind::NotYetImplemented {
                                    construct: format!(
                                        "`HashMap<{}, {}>` {role} clone",
                                        key_ty.user_facing(),
                                        val_ty.user_facing()
                                    ),
                                    site: expr.site,
                                },
                                note: format!(
                                    "{operation}, but its {role} {reason}; the concrete generic \
                                     instantiation is rejected before the runtime clone choke"
                                ),
                            });
                            return None;
                        }
                    }
                }

                // HashSet clone and to-Vec projection duplicate every element.
                // Plain `for x in set` synthesizes `to_vec`, while `remove`
                // moves an element out and deliberately does not enter here.
                if matches!(
                    callee.as_str(),
                    "hew_hashset_clone_layout" | "hew_hashset_to_vec_layout"
                ) {
                    let concrete_receiver = self.subst_ty(&receiver.ty);
                    let ResolvedTy::Named {
                        args: receiver_args,
                        builtin: Some(hew_types::BuiltinType::HashSet),
                        ..
                    } = &concrete_receiver
                    else {
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct: "HashSet clone on a non-HashSet receiver".to_string(),
                                site: expr.site,
                            },
                            note: format!(
                                "`{callee}` requires a concrete `HashSet<T>` receiver, but MIR \
                                 substitution produced `{concrete_receiver}`"
                            ),
                        });
                        return None;
                    };
                    let [elem_ty] = receiver_args.as_slice() else {
                        unreachable!("a resolved HashSet receiver always carries one argument");
                    };
                    if let Err(reason) = self.validate_collection_clone_value(elem_ty) {
                        let operation = if callee == "hew_hashset_clone_layout" {
                            "HashSet.clone() must duplicate every element"
                        } else {
                            "HashSet.to_vec() must clone every element into an independent snapshot"
                        };
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct: format!(
                                    "`HashSet<{}>` element clone",
                                    elem_ty.user_facing()
                                ),
                                site: expr.site,
                            },
                            note: format!(
                                "{operation}, but {reason}; the concrete generic instantiation \
                                 is rejected before the runtime clone choke"
                            ),
                        });
                        return None;
                    }
                }

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
                // This synthetic-array call's exact known move ABI transfers
                // (rather than clone-copies) its element argument into the Vec
                // descriptor slot. An unknown call, normal `Vec::push`, and
                // every ordinary copy-in path remain source-owning.
                let vec_owned_move_array_ingress =
                    is_array_literal_push && callee == "hew_vec_push_owned_move";

                // Lower receiver as arg[0], then explicit args.
                let receiver_place = self.lower_value(receiver)?;
                let mut arg_places = vec![receiver_place];
                let mut yield_retained_locals: Vec<u32> = Vec::new();
                for (arg_index, arg) in args.iter().enumerate() {
                    // A trait-object element push is the one rewrite that turns
                    // a BARE-BINDING push into `hew_vec_push_owned_move` (the
                    // drop-only descriptor has no clone thunk for copy-in), so
                    // the source binding must be consumed through the move
                    // ingress here. The scope is structural trait-object-ness,
                    // never the `_move` spelling alone: the array-literal
                    // desugar owns its element consume
                    // (`consume_owned_vec_move_array_element` below), and the
                    // fresh-rvalue `_move` rewrite for other owned elements
                    // moves an unbound temp with no binding to consume — a
                    // second move-ingress consume on either would double-record
                    // the same site as use-after-consume.
                    let move_only_vec_ingress = self
                        .vec_receiver_has_drop_only_element(&receiver.ty)
                        && !vec_owned_move_array_ingress
                        && matches!(
                            (target_family, callee.as_str(), arg_index),
                            (
                                hew_types::MethodTargetFamily::Vec(hew_types::VecMethod::Push),
                                "hew_vec_push_owned_move",
                                0
                            ) | (
                                hew_types::MethodTargetFamily::Vec(hew_types::VecMethod::Set),
                                "hew_vec_set_owned_move",
                                1
                            )
                        );
                    let move_ingress =
                        builtin_method_arg_is_move_ingress(*target_family) || move_only_vec_ingress;
                    // `HashMap`/`HashSet` ingress is MOVE by ABI — the runtime
                    // documents copy-in as intentionally absent — so the operand's
                    // heap is byte-transferred into the collection and the
                    // collection's compiler-scheduled teardown releases it through
                    // the value layout's `drop_fn`. Deciding that from the method
                    // FAMILY alone schedules a release of a value the compiler may
                    // not own: `m.insert(k, wrap())` hands the map an
                    // ownership-opaque foreign record and the teardown frees a
                    // handle the host still holds.
                    //
                    // Unlike the `Vec` seam there is no COPY-IN sibling to fall
                    // back to, so failing closed cannot mean "mint nothing" — the
                    // move happens either way. It means refusing to compile the
                    // ingress at all, which is what this does.
                    if move_ingress && self.reject_opaque_foreign_collection_ingress(arg) {
                        return None;
                    }
                    // Catalogued runtime collection sinks adopt a consuming
                    // payload only after the invoke returns normally. Keep an
                    // affine binding's guard and OwnerId live while lowering
                    // this exact argument site; `splice_normal_call_ownership_commits`
                    // remains the single normal-edge transfer authority. A
                    // direct Hew consuming parameter does not enter this path
                    // and continues to transfer before invoke.
                    let runtime_defers_affine_consume =
                        runtime_authority_for_collection(*target_family, &callee).is_some_and(
                            |family| {
                                family.arg_consume_verdict(arg_index + 1)
                                    == hew_types::runtime_call::ConsumeVerdict::ProvenConsume
                            },
                        );
                    if runtime_defers_affine_consume {
                        self.deferred_affine_call_consume_sites.insert(arg.site);
                    }
                    let lowered_arg =
                        self.lower_method_arg_value(arg, is_vec_element_store || move_ingress);
                    if runtime_defers_affine_consume {
                        self.deferred_affine_call_consume_sites.remove(&arg.site);
                    }
                    let arg_place = lowered_arg?;
                    arg_places.push(arg_place);
                    if move_ingress
                        && !self.retain_caller_borrowed_cow_collection_ingress(arg, arg_place)
                        && !self.retain_yield_binder_cow_collection_ingress(
                            arg,
                            arg_place,
                            &mut yield_retained_locals,
                        )
                    {
                        self.consume_moved_builtin_method_arg(arg);
                    }
                    if vec_owned_move_array_ingress {
                        self.consume_owned_vec_move_array_element(arg);
                    } else if is_array_literal_push {
                        self.alias_moved_owned_operand(arg);
                    }
                    if is_vec_element_store {
                        self.enforce_closure_pair_ingress(arg);
                    }
                }
                // COPY-IN param embeds stay caller-borrowed; only the source
                // temp's independently retained string share gains an owner.
                self.finalize_vec_copy_in_source_owner(&callee, args, &arg_places);
                let receiver_contract = crate::runtime_symbols::callee_ownership_contract(&callee);
                if receiver_contract.borrows_vec_receiver()
                    || receiver_contract.borrows_collection_receiver()
                {
                    self.finalize_borrowed_receiver_owner(receiver, receiver_place);
                }
                let dest = if matches!(ret_ty, ResolvedTy::Unit) {
                    None
                } else {
                    Some(self.alloc_local(ret_ty.clone()))
                };
                let next = self.alloc_block();
                // The checked collection verdict selected this concrete ABI.
                // Preserve both the verdict and its derived runtime family;
                // structural clone/move chokes are an explicit compiler-owned
                // exception, never a spelling-based codegen fallback.
                let authority = match callee.as_str() {
                    "hew_hashmap_get_clone_layout" => crate::CallAuthority::Compiler(
                        crate::CompilerCallKind::HashMapGetCloneLayoutOption,
                    ),
                    "hew_hashmap_remove_take_layout" => crate::CallAuthority::Compiler(
                        crate::CompilerCallKind::HashMapRemoveTakeLayout,
                    ),
                    _ => {
                        if let Some(kind) = closure_pair_vec_kind(
                            *target_family,
                            &callee,
                            &self.subst_ty(&receiver.ty),
                        ) {
                            crate::CallAuthority::Compiler(crate::CompilerCallKind::ClosurePairVec(
                                kind,
                            ))
                        } else {
                            let Some(runtime) =
                                runtime_authority_for_collection(*target_family, &callee)
                            else {
                                self.diagnostics.push(MirDiagnostic {
                                kind: MirDiagnosticKind::NotYetImplemented {
                                    construct: format!(
                                        "collection call `{callee}` has no runtime authority"
                                    ),
                                    site: expr.site,
                                },
                                note: "the checked collection verdict must materialise either a \
                                       catalogued runtime family or an explicit compiler structural \
                                       call kind before codegen".to_string(),
                            });
                                return None;
                            };
                            crate::CallAuthority::Runtime(runtime)
                        }
                    }
                };
                // A pre-retained yield-binder ingress operand makes this
                // consuming Call a borrow of the binder's own count — record
                // the exemption so the body-end drop-safety scan does not
                // suppress the binder's per-iteration release.
                for local in &yield_retained_locals {
                    self.yield_share_term_exempt
                        .insert((self.current_block_id, *local));
                }
                self.finish_current_block(Terminator::Call {
                    callee,
                    authority,
                    args: arg_places,
                    dest,
                    next,
                });
                self.start_block(next);
                dest
            }
            HirExprKind::CallTraitMethodStatic {
                receiver,
                target,
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
                            "static trait dispatch `{declaring_trait}.{method_name}` reached \
                             MIR in a concrete function body without a substitution for \
                             receiver type parameter `{receiver_type_param}`; this indicates \
                             a missing monomorphization binding (the generic origin should \
                             not be emitted)"
                        ),
                    });
                    return None;
                };
                // (2) canonical nominal instance.
                let Some(self_type) =
                    hew_hir::dispatch::receiver_self_type_for_impl_lookup_instance(&concrete_ty)
                else {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::NotYetImplemented {
                            construct: format!(
                                "static trait dispatch on receiver shape `{concrete_ty:?}` \
                                 for `{declaring_trait}.{method_name}`"
                            ),
                            site: expr.site,
                        },
                        note: "receiver type has no canonical impl-self name; \
                               static dispatch supports nominal and primitive receivers only"
                            .to_string(),
                    });
                    return None;
                };
                let hew_types::CallTarget::StaticTraitMethod {
                    declaring_trait: target_trait,
                    method: target_method,
                } = target
                else {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::UnsupportedNode {
                            reason: "static trait call has no executable checker target"
                                .to_string(),
                        },
                        note: "HIR must reject unsupported static-trait targets before MIR"
                            .to_string(),
                    });
                    return None;
                };
                if let Some(callee) =
                    primitive_display_static_callee(target, &concrete_ty, args.len())
                {
                    return self.lower_direct_call_with_authority(
                        callee,
                        None,
                        std::slice::from_ref(receiver),
                        &resolved_ret_ty,
                        expr.site,
                        catalog_display_call_authority(callee),
                    );
                }
                // (3) structured registry lookup by checker-owned IDs. The
                // only fallback inside the HIR index is the exact same nominal's
                // generic implementation; there is no string/leaf retry.
                let Some(entry) = hew_hir::dispatch::lookup_trait_impl_entry_by_id(
                    &self.trait_impl_index,
                    target_trait,
                    &self_type,
                    target_method,
                )
                .cloned() else {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::StaticDispatchImplNotFound {
                            declaring_trait: declaring_trait.clone(),
                            self_type_name: self_type.nominal.declaration().full_path().to_string(),
                            method_name: method_name.clone(),
                            site: expr.site,
                        },
                        note: format!(
                            "no impl of trait `{}` for `{}` \
                             registered in the static-dispatch index; the checker should \
                             have rejected this call",
                            target_trait.full_path(),
                            self_type.nominal.declaration().full_path(),
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
                                self_type_name: self_type
                                    .nominal
                                    .declaration()
                                    .full_path()
                                    .to_string(),
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
                    let mangled = hew_hir::monomorph::function_monomorph_symbol(
                        &entry.method_symbol,
                        &self_type.args,
                    );
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
                    authority: crate::model::CallAuthority::default(),
                    args: arg_places,
                    dest,
                    next,
                });
                self.start_block(next);
                dest
            }
            HirExprKind::VarSelfMethodCall {
                receiver,
                call_target,
                target,
                args,
                ret_ty,
                receiver_ty,
                ..
            } => self.lower_var_self_method_call(
                expr.site,
                receiver,
                call_target,
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
                let dest = self.alloc_local(expr.ty.clone());
                let Place::Local(dest_local) = dest else {
                    unreachable!("alloc_local returns Place::Local");
                };
                let mut lowered_fields = Vec::new();
                if let Some(fields) = payload {
                    for (field_idx, (_field_name, field_expr)) in fields.iter().enumerate() {
                        let Some(src) = self.lower_value_for_move(field_expr) else {
                            continue;
                        };
                        let field_idx =
                            u32::try_from(field_idx).expect("field index exceeds u32::MAX");
                        let variant_idx =
                            u32::try_from(*state_idx).expect("state index exceeds u32::MAX");
                        lowered_fields.push((field_expr, field_idx, variant_idx, src));
                    }
                }
                let tag_const = self.alloc_local(ResolvedTy::I64);
                self.push_instr(Instr::ConstI64 {
                    dest: tag_const,
                    value: i64::try_from(*state_idx).unwrap_or(i64::MAX),
                });
                self.push_instr(Instr::Move {
                    dest: Place::MachineTag(dest_local),
                    src: tag_const,
                });
                for (field_expr, field_idx, variant_idx, src) in lowered_fields {
                    self.push_instr(Instr::Move {
                        dest: Place::MachineVariant {
                            local: dest_local,
                            variant_idx,
                            field_idx,
                        },
                        src,
                    });
                    self.alias_moved_owned_operand(field_expr);
                    self.enforce_closure_pair_ingress(field_expr);
                }
                Some(dest)
            }
            HirExprKind::MachineFieldAccess {
                machine_name,
                state_idx,
                field_idx,
                field_name,
                ..
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
                ..
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
                let step_layout_key = match &receiver.ty {
                    // The machine-mono registry and synthetic step emitter
                    // both use the class-tagged machine layout key.  A plain
                    // named-layout key drops that class/owner authority and
                    // sends imported or generic machines to an undeclared
                    // `<leaf>__step` symbol.
                    ResolvedTy::Named { name, args, .. } => hew_hir::machine_layout_key(name, args),
                    _ => machine_name.clone(),
                };
                self.finish_current_block(Terminator::Call {
                    callee: mangle_machine_step(&step_layout_key),
                    authority: crate::model::CallAuthority::default(),
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
                        handoff: ActorStateStoreHandoff::ConsumeSource,
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
                    // State-name tables are emitted per concrete machine
                    // layout, not per declaration leaf.  Carry the same
                    // class-tagged instance key used by `.step()` so generic
                    // imported machines select `mc$$owner$$Machine$$T`.
                    machine_name: match &receiver.ty {
                        ResolvedTy::Named { name, args, .. } => {
                            hew_hir::machine_layout_key(name, args)
                        }
                        _ => machine_name.clone(),
                    },
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
                // The emit-push side (`machine_synth.rs::synthesize_machine_step_fn`)
                // hashes the class-tagged machine layout key, not the plain
                // declared name — mirror the same `MachineStateName` pattern
                // above so `take_emits` hashes the identical key `emit()`
                // tagged its push with. A plain-name hash here would produce
                // a different SipHash digest and silently never match a
                // pushed emit (machine_emit_type_id is `SipHasher13` over
                // the name bytes).
                let emit_layout_key = match &receiver.ty {
                    ResolvedTy::Named { name, args, .. } => hew_hir::machine_layout_key(name, args),
                    _ => machine_name.clone(),
                };
                self.push_instr(Instr::MachineEmitTake {
                    machine_emit_id: machine_emit_type_id(&emit_layout_key),
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
            HirExprKind::Match { scrutinee, arms } => {
                self.lower_match(expr.site, scrutinee, arms, &expr.ty)
            }
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
                // Release every `for x in …` snapshot cursor this break
                // abandons. Bounded to the broken loop's window, so a cursor
                // whose own loop is being broken is EXCLUDED (its desugar block
                // encloses the loop and its fall-through close is the single
                // release); only a cursor inside an ENCLOSING loop's window —
                // `break @outer` from a nested `for` — is released here.
                self.emit_vec_iter_drops_for_exit_edge(frame.scope_depth);
                let exited_scopes = self.active_scopes[frame.scope_depth..].to_vec();
                self.emit_scope_exit_marker(exited_scopes);
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
                    let value_place = self.lower_value_for_move(expr_value);
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
                // A while-let call scrutinee is an active, path-local
                // generation.  Returning from the body bypasses its normal
                // back-edge/false-edge release, so consume that exact owner on
                // this exit edge before sealing the block.
                self.record_active_iteration_owner_drops_for_exit_edge(0);
                self.emit_stream_drops_for_exit_edge(0);
                // Release every `for x in …` snapshot cursor this return
                // abandons — same discipline as the statement-position return.
                // This is the edge the `?` desugar's `return Err(e)` arm takes.
                self.emit_vec_iter_drops_for_exit_edge(0);
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
                // Release every `for x in …` snapshot cursor this continue
                // abandons — `continue @outer` from a nested `for` restarts the
                // outer loop past the inner cursor's fall-through close. The
                // window EXCLUDES a cursor whose own loop is being continued:
                // that cursor is still mid-iteration and must stay live.
                self.emit_vec_iter_drops_for_exit_edge(frame.scope_depth);
                // Register THIS block as a loop back-edge so `enumerate_exits`
                // populates its `Goto` `DropPlan` with the scope-filtered
                // releases for body-scope heap-owning bindings (a live
                // `let opt = rx.try_recv()` carried into the next iteration
                // would otherwise leak its `Option<string>` payload because
                // the body-end Drop sits past the continue terminator and is
                // skipped). The fall-through back-edge is registered at the
                // bottom of each `lower_*` loop; this is the analogous
                // registration for the explicit-continue exit path.
                let exited_scopes = self.active_scopes[frame.scope_depth..]
                    .iter()
                    .copied()
                    .collect::<HashSet<_>>();
                self.emit_scope_exit_marker(exited_scopes.iter().copied());
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
                    self.push_bind_statement(
                        companion,
                        companion_name.clone(),
                        expr.site,
                        companion_ty.clone(),
                    );
                    // Wire `binding_locals` BEFORE `register_owned_local`:
                    // `register_owned_local` reads the slot to classify
                    // ownership, and drop elaboration resolves the drop place
                    // from `binding_locals` at exit. A missing slot silently
                    // defaults to `Place::Local(0)` — dropping the WRONG local.
                    self.binding_locals.insert(companion, gen_place);
                    self.record_binding_scope(companion);
                    // The companion owns the coro handle THIS expression just
                    // materialised, so the gen-block expression is the value the
                    // provenance question is about.
                    let warrant = self.owner_warrant_for_admitted_temp(expr);
                    self.register_owned_local(companion, companion_name, companion_ty, warrant);
                    self.build_stream_producer_pump(gen_place, &pump, expr.site);
                    None
                } else {
                    Some(gen_place)
                }
            }
            HirExprKind::Yield { value, yield_ty: _ } => {
                self.lower_yield_expr(expr, value.as_deref())
            }
            HirExprKind::SubsumedValue { source, .. } => self.lower_value_inner(source),
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
                        authority: crate::model::CallAuthority::default(),
                        args: vec![src_place],
                        dest: Some(dest),
                        next,
                    });
                    self.start_block(next);
                    return Some(dest);
                }
                if matches!(
                    record_ty,
                    ResolvedTy::Tuple(_)
                        | ResolvedTy::Named {
                            builtin: Some(BuiltinType::Option | BuiltinType::Result),
                            ..
                        }
                ) {
                    let record_layouts: Vec<crate::model::RecordLayout> = self
                        .record_field_orders
                        .iter()
                        .filter(|(_, fields)| !fields.is_empty())
                        .map(|(name, fields)| crate::model::RecordLayout {
                            name: name.clone(),
                            field_tys: fields.iter().map(|(_, ty)| ty.clone()).collect(),
                            field_names: fields.iter().map(|(field, _)| field.clone()).collect(),
                        })
                        .collect();
                    let plan =
                        match crate::state_clone::classify_value_snapshot_plan_with_lifecycle_registry(
                            &record_ty,
                            &record_layouts,
                            &self.enum_layouts,
                            &self.opaque_handle_names,
                            &self.lifecycle_registry,
                        ) {
                            Ok(plan) => plan,
                            Err(error) => {
                                self.diagnostics.push(MirDiagnostic {
                                    kind: MirDiagnosticKind::NotYetImplemented {
                                        construct: format!(
                                            "structural clone of `{record_ty}` could not be \
                                             classified: {error}"
                                        ),
                                        site: expr.site,
                                    },
                                    note: "the checker admitted the clone, but MIR could not build \
                                           a total member-wise clone plan"
                                        .to_string(),
                                });
                                return None;
                            }
                        };
                    match plan.is_clone_total(
                        &record_layouts,
                        &self.enum_layouts,
                        &self.opaque_handle_names,
                        &self.lifecycle_registry,
                    ) {
                        Ok(true) => {}
                        Ok(false) => {
                            self.diagnostics.push(MirDiagnostic {
                                kind: MirDiagnosticKind::NotYetImplemented {
                                    construct: format!(
                                        "structural clone of `{record_ty}` contains a drop-only \
                                         member"
                                    ),
                                    site: expr.site,
                                },
                                note: "clone admission must never manufacture ownership for an \
                                       affine or resource-bearing member"
                                    .to_string(),
                            });
                            return None;
                        }
                        Err(error) => {
                            self.diagnostics.push(MirDiagnostic {
                                kind: MirDiagnosticKind::NotYetImplemented {
                                    construct: format!(
                                        "structural clone totality for `{record_ty}` could not be \
                                         proven: {error}"
                                    ),
                                    site: expr.site,
                                },
                                note: "MIR requires a total clone and inverse drop plan"
                                    .to_string(),
                            });
                            return None;
                        }
                    }
                    let dest = self.alloc_local(record_ty.clone());
                    self.instructions.push(Instr::ValueSnapshotClone {
                        dest,
                        src: src_place,
                        ty: record_ty,
                        plan,
                        boundary: crate::model::PreparedCarrierBoundary::LocalCall,
                    });
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

    /// Materialise the checker-selected common type for a pair of integer
    /// operands. HIR records the common result type, but each child expression
    /// still carries its independently resolved type; without this boundary a
    /// legal `i64 + i32` reaches codegen as two different LLVM integer widths.
    ///
    /// This mirrors `hew-types::check::coerce::common_integer_type` for the
    /// post-checker `ResolvedTy` domain: fixed-width integers of matching
    /// signedness choose the wider type, while platform-sized integers combine
    /// only with their exact own type. Invalid combinations fail closed rather
    /// than manufacturing an implicit cast the checker does not admit.
    fn normalize_integer_binary_operands(
        &mut self,
        lhs: Place,
        rhs: Place,
        lhs_ty: &ResolvedTy,
        rhs_ty: &ResolvedTy,
        result_ty: Option<&ResolvedTy>,
        site: hew_hir::SiteId,
    ) -> Option<(Place, Place, ResolvedTy)> {
        debug_assert!(lhs_ty.is_integer() && rhs_ty.is_integer());

        let common_ty = if lhs_ty == rhs_ty {
            lhs_ty.clone()
        } else {
            if matches!(lhs_ty, ResolvedTy::Isize | ResolvedTy::Usize)
                || matches!(rhs_ty, ResolvedTy::Isize | ResolvedTy::Usize)
            {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!(
                            "implicit integer coercion between `{}` and `{}`",
                            lhs_ty.user_facing(),
                            rhs_ty.user_facing()
                        ),
                        site,
                    },
                    note: "platform-sized integers combine only with the exact same type; use an explicit conversion"
                        .to_string(),
                });
                return None;
            }
            let lhs_sign = integer_signedness(lhs_ty);
            let rhs_sign = integer_signedness(rhs_ty);
            let lhs_width = integer_bit_width(lhs_ty, self.pointer_width);
            let rhs_width = integer_bit_width(rhs_ty, self.pointer_width);
            if lhs_sign.is_none()
                || lhs_sign != rhs_sign
                || lhs_width.is_none()
                || rhs_width.is_none()
            {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!(
                            "implicit integer coercion between `{}` and `{}`",
                            lhs_ty.user_facing(),
                            rhs_ty.user_facing()
                        ),
                        site,
                    },
                    note: "integer operands must have compatible signedness and width; use an explicit conversion"
                        .to_string(),
                });
                return None;
            }
            if lhs_width >= rhs_width {
                lhs_ty.clone()
            } else {
                rhs_ty.clone()
            }
        };

        if let Some(result_ty) = result_ty {
            if result_ty != &common_ty {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::UnsupportedNode {
                        reason: format!(
                            "integer binary result `{}` disagrees with common operand type `{}`",
                            result_ty.user_facing(),
                            common_ty.user_facing()
                        ),
                    },
                    note: "the checker and MIR integer-coercion authorities must select the same result type"
                        .to_string(),
                });
                return None;
            }
        }

        let mut cast_operand = |src: Place, from_ty: &ResolvedTy| {
            if from_ty == &common_ty {
                return src;
            }
            let dest = self.alloc_local(common_ty.clone());
            self.push_instr(Instr::NumericCast {
                dest,
                src,
                from_ty: from_ty.clone(),
                to_ty: common_ty.clone(),
            });
            dest
        };
        let lhs = cast_operand(lhs, lhs_ty);
        let rhs = cast_operand(rhs, rhs_ty);
        Some((lhs, rhs, common_ty))
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
        let lhs_ty = self.subst_ty(lhs_ty);
        let rhs_ty = self.subst_ty(rhs_ty);
        let ty = self.subst_ty(ty);
        let dest = self.alloc_local(ty.clone());

        // One post-checker coercion authority feeds every integer binary MIR
        // instruction. Comparisons have a bool result and therefore derive the
        // common type solely from their operands; all other integer operators
        // additionally prove that HIR's result type is that same common type.
        let is_comparison = matches!(
            op,
            BinaryOp::Equal
                | BinaryOp::NotEqual
                | BinaryOp::Less
                | BinaryOp::LessEqual
                | BinaryOp::Greater
                | BinaryOp::GreaterEqual
        );
        let (lhs, rhs) = if lhs_ty.is_integer() && rhs_ty.is_integer() {
            let expected_result = (!is_comparison).then_some(&ty);
            let Some((lhs, rhs, _common_ty)) = self.normalize_integer_binary_operands(
                lhs,
                rhs,
                &lhs_ty,
                &rhs_ty,
                expected_result,
                site,
            ) else {
                // No cast locals are allocated on the reject path, so the
                // destination remains the last local and can be rolled back.
                self.locals.pop();
                return None;
            };
            (lhs, rhs)
        } else {
            (lhs, rhs)
        };
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
                return self.lower_div_rem(op, dest, lhs, rhs, &ty, site);
            }
            BinaryOp::Shl | BinaryOp::Shr => {
                return self.lower_shift(op, dest, lhs, rhs, &ty, site);
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
        if let Some(width) = float_width(&ty) {
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

        if matches!(op, BinaryOp::Add) && matches!(&ty, ResolvedTy::String) {
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
        let Some(signed) = integer_signedness(&ty) else {
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
        let then_value = self.lower_composite_result_value(then_expr);
        // A nested divergent expression still has a placeholder result place
        // so its enclosing HIR node can keep a uniform value shape.  Its
        // cursor, however, is unreachable: do not attempt to coerce that
        // placeholder (often `!`) into this `if`'s checker-selected join
        // type or emit a dead Move.
        if !self.cursor_unreachable && else_expr.is_some() {
            if let Some(src) = then_value {
                let src = self.normalize_checker_numeric_value(
                    src,
                    &then_expr.ty,
                    result_ty,
                    "if then branch",
                    then_expr.site,
                )?;
                self.push_composite_result_move(result_place, src, result_ty);
            }
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
            let else_value = self.lower_composite_result_value(else_expr);
            if !self.cursor_unreachable {
                if let Some(src) = else_value {
                    let src = self.normalize_checker_numeric_value(
                        src,
                        &else_expr.ty,
                        result_ty,
                        "if else branch",
                        else_expr.site,
                    )?;
                    self.push_composite_result_move(result_place, src, result_ty);
                }
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
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        if let Err(reason) = self.validate_collection_clone_value(elem_ty) {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!(
                        "`HashMap<_, {}>` indexed value clone",
                        elem_ty.user_facing()
                    ),
                    site,
                },
                note: format!(
                    "HashMap indexing must clone the matched value into an independent owner, \
                     but it {reason}; the access is rejected before the runtime clone choke"
                ),
            });
            return None;
        }
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
            authority: crate::CallAuthority::Compiler(
                crate::CompilerCallKind::HashMapGetCloneLayoutIndex,
            ),
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

        if matches!(elem_ty, ResolvedTy::TraitObject { .. })
            || matches!(
                elem_ty,
                ResolvedTy::Named {
                    builtin: Some(hew_types::BuiltinType::Receiver),
                    ..
                }
            )
        {
            return self.reject_drop_only_vec_operation("range slice", site);
        }

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
    #[expect(
        clippy::too_many_lines,
        reason = "the fail-closed static dispatch gate keeps all exact-identity rejection paths together"
    )]
    fn resolve_static_trait_method_callee(
        &mut self,
        receiver_type_param: &str,
        target: &hew_types::CallTarget,
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
                    "static trait dispatch `{declaring_trait}.{method_name}` reached \
                     MIR in a concrete function body without a substitution for \
                     receiver type parameter `{receiver_type_param}`; this indicates \
                     a missing monomorphization binding (the generic origin should \
                     not be emitted)"
                ),
            });
            return None;
        };
        let Some(self_type) =
            hew_hir::dispatch::receiver_self_type_for_impl_lookup_instance(&concrete_ty)
        else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!(
                        "static trait dispatch on receiver shape `{concrete_ty:?}` \
                         for `{declaring_trait}.{method_name}`"
                    ),
                    site,
                },
                note: "receiver type has no canonical impl-self name; \
                       static dispatch supports nominal and primitive receivers only"
                    .to_string(),
            });
            return None;
        };
        let hew_types::CallTarget::StaticTraitMethod {
            declaring_trait: target_trait,
            method: target_method,
        } = target
        else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::UnsupportedNode {
                    reason: "var-self static trait call has no executable checker target"
                        .to_string(),
                },
                note: "HIR must reject unsupported static-trait targets before MIR".to_string(),
            });
            return None;
        };
        let Some(entry) = hew_hir::dispatch::lookup_trait_impl_entry_by_id(
            &self.trait_impl_index,
            target_trait,
            &self_type,
            target_method,
        )
        .cloned() else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::StaticDispatchImplNotFound {
                    declaring_trait: declaring_trait.to_string(),
                    self_type_name: self_type.nominal.declaration().full_path().to_string(),
                    method_name: method_name.to_string(),
                    site,
                },
                note: format!(
                    "no impl of trait `{}` for `{}` \
                     registered in the static-dispatch index; the checker should \
                     have rejected this call",
                    target_trait.full_path(),
                    self_type.nominal.declaration().full_path(),
                ),
            });
            return None;
        };
        if entry.impl_type_params.is_empty() {
            if !self.module_fn_names.contains(&entry.method_symbol) {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::StaticDispatchImplNotFound {
                        declaring_trait: declaring_trait.to_string(),
                        self_type_name: self_type.nominal.declaration().full_path().to_string(),
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
        let mangled =
            hew_hir::monomorph::function_monomorph_symbol(&entry.method_symbol, &self_type.args);
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

    /// Project a checker-selected HIR body symbol to the exact emitted body at
    /// one call site.  The declaration-to-symbol lookup happens at the caller;
    /// this helper only applies its carried concrete type arguments.  Keeping
    /// the projection shared by `User` and `ImplMethod` calls prevents generic
    /// impl dispatch from falling back to a receiver/display-name reconstruction.
    fn project_direct_call_symbol(&self, symbol: String, site: SiteId) -> String {
        let Some(type_args) = self.call_site_type_args.get(&site) else {
            return symbol;
        };
        let substituted: Vec<ResolvedTy> = type_args.iter().map(|ty| self.subst_ty(ty)).collect();
        hew_hir::monomorph::function_monomorph_symbol(&symbol, &substituted)
    }

    fn resolve_var_self_direct_callee(
        &mut self,
        call_target: &hew_types::CallTarget,
        site: SiteId,
        receiver_ty: &ResolvedTy,
    ) -> Option<String> {
        let hew_types::CallTarget::ImplMethod(declaration) = call_target else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::UnsupportedNode {
                    reason: format!(
                        "var-self direct call has non-impl checker target {call_target:?}"
                    ),
                },
                note: "a direct var-self call requires a checker-owned ImplMethod declaration"
                    .to_string(),
            });
            return None;
        };
        let Some(base_callee) = self.direct_call_symbols.get(declaration).cloned() else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!(
                        "var-self impl method `{}` without an HIR symbol map",
                        declaration.full_path()
                    ),
                    site,
                },
                note: "MIR will not reconstruct a var-self method endpoint from `Type.method` text"
                    .to_string(),
            });
            return None;
        };
        // Generic impl/method origins are deliberately absent from
        // `module_fn_names`; their type arguments are applied only after the
        // exact declaration-to-symbol projection above.
        let callee = self.project_direct_call_symbol(base_callee.clone(), site);
        if self.module_fn_names.contains(&callee) {
            return Some(callee);
        }
        let substituted_receiver = self.subst_ty(receiver_ty);
        if let ResolvedTy::Named { args, .. } = &substituted_receiver {
            if !args.is_empty() {
                let mangled = hew_hir::monomorph::function_monomorph_symbol(&base_callee, args);
                if self.module_fn_names.contains(&mangled) {
                    return Some(mangled);
                }
            }
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
        self.push_bind_statement(binding_id, name.to_string(), site, ty.clone());
        if self.binding_seeds_drop_elaboration(ty)
            && !self.owned_locals.iter().any(|entry| {
                entry.binding == binding_id && entry.disposition == Disposition::ScopeExit
            })
        {
            // U1 tail — the `var`-self receiver restore re-mints an owner for a
            // binding that already exists in this frame. Ask the ledger about
            // that same binding: one the `let` binder refused an owner for must
            // not acquire one by being written back.
            let warrant = self.owner_warrant_for_rebind(binding_id, binding_id, ty);
            self.register_owned_local(binding_id, name.to_string(), ty.clone(), warrant);
        }
    }

    fn var_self_receiver_slot(&mut self, receiver: &HirExpr) -> Option<(BindingId, String, Place)> {
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
        Some((*binding_id, receiver_name.clone(), receiver_slot))
    }

    #[expect(
        clippy::too_many_arguments,
        reason = "the var-self carrier is intentionally explicit across site, target, receiver, result, and writeback types"
    )]
    fn lower_var_self_method_call(
        &mut self,
        site: SiteId,
        receiver: &HirExpr,
        call_target: &hew_types::CallTarget,
        target: &HirVarSelfMethodTarget,
        args: &[HirExpr],
        ret_ty: &ResolvedTy,
        receiver_ty: &ResolvedTy,
    ) -> Option<Place> {
        let (binding_id, receiver_name, receiver_slot) = self.var_self_receiver_slot(receiver)?;
        // Generic `Iterator::next(var self)` calls retain their static-trait
        // HIR shape until the enclosing function is monomorphised.  Direct
        // VecIter syntax was already rewritten by HIR, but the same concrete
        // cursor reached through `std::iter::fold<I: Iterator>` arrives here.
        // Intercept only the exact builtin declaration identity; all other
        // static var-self calls still use the declaration-keyed impl registry.
        if let HirVarSelfMethodTarget::StaticTrait {
            receiver_type_param,
            ..
        } = target
        {
            if let VecIterStaticNextLowering::Lowered(result) = self
                .lower_builtin_vec_iter_static_next_if_applicable(
                    receiver_type_param,
                    receiver,
                    call_target,
                    args,
                    ret_ty,
                    site,
                )
            {
                return result;
            }
        }
        let callee_symbol = match target {
            HirVarSelfMethodTarget::Direct => {
                self.resolve_var_self_direct_callee(call_target, site, receiver_ty)?
            }
            HirVarSelfMethodTarget::StaticTrait {
                receiver_type_param,
                declaring_trait,
                method_name,
                ..
            } => self.resolve_static_trait_method_callee(
                receiver_type_param,
                call_target,
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
            authority: crate::model::CallAuthority::default(),
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
            binding_id,
            &receiver_name,
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
        callee_item: Option<hew_hir::ItemId>,
        hir_args: &[hew_hir::HirExpr],
        ret_ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        self.lower_direct_call_with_authority(
            callee_symbol,
            callee_item,
            hir_args,
            ret_ty,
            site,
            builtin
                .map(crate::CallAuthority::Runtime)
                .unwrap_or_default(),
        )
    }

    /// Lower a direct call using the checker/HIR-projected authority. The
    /// `Extern` variant is the capability to read an audited FFI parameter
    /// contract; it does not select a specialised codegen ABI.
    fn lower_direct_call_with_authority(
        &mut self,
        callee_symbol: &str,
        callee_item: Option<hew_hir::ItemId>,
        hir_args: &[hew_hir::HirExpr],
        ret_ty: &ResolvedTy,
        site: hew_hir::SiteId,
        authority: crate::CallAuthority,
    ) -> Option<Place> {
        let builtin = authority.runtime_family();
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
        if self.reject_opaque_foreign_callable_result(callee_symbol, ret_ty, site) {
            return None;
        }
        // U3 / U9 preflight, BEFORE any argument lowering so a refusal leaves no
        // partial MIR — the same posture the #2648 scrutinee reject takes. A
        // callee-owned parameter mints its scope-exit owner from the parameter's
        // TYPE inside the callee, a frame with no expression to ask about; this
        // is where the question is answerable, so this is where it is asked.
        if self.reject_opaque_foreign_call_arg_transfers(callee_item, hir_args) {
            return None;
        }
        // Keep declared-extern affine consumes caller-owned through the call.
        // Their exact HIR sites preserve `Use { Consume }` while deferring the
        // guard, physical neutralization, and `OwnerId` commit to the normal
        // successor. Direct Hew consumes take the ordinary binding path: the
        // caller transfers before invoke and the callee owns from entry.
        let pending_affine_consumes =
            self.affine_call_consume_candidates(callee_symbol, callee_item, hir_args);
        self.activate_affine_call_consume_sites(&pending_affine_consumes);

        // Lower each argument left-to-right. If any fails to produce a Place,
        // fail the whole call after retiring the transient site context —
        // argument diagnostics already capture the root cause.
        let lowered_args = self.lower_direct_call_args(callee_symbol, callee_item, hir_args);
        self.deactivate_affine_call_consume_sites(&pending_affine_consumes);
        let arg_places = lowered_args?;

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
        // #2743 — complete the caller-side owner handoff for every typed owned
        // composite/string argument TEMPORARY passed to a BORROWING parameter.
        // The temporary has no user `let`, so its publication owner is the one
        // exact generation that must reach scope-exit planning. This sink only
        // changes that owner's structural role; it never reclassifies or remints
        // the value.
        //
        // Exactly-once gate is per type, aligned with the prover's own
        // borrow-vs-consume exemption:
        //  - record / tuple / enum: BORROW iff the arg site is in
        //    `proven_borrow_args` (the same `proven_borrow_call_args` exemption
        //    the composite provers read). A CONSUMING composite callee's temp is
        //    NOT registered here (its arg is absent from `proven_borrow_args`); the
        //    callee owns and drops it (#2732 for enums) — mutually exclusive.
        //  - string: handed off iff the callee is a USER free function (a string
        //    param is never recorded in `proven_borrow_arg_sites` — its borrow
        //    model is the separate refcount contract). The string sole-owner
        //    prover then gates the actual drop exactly as for the named
        //    `let s = a+b; h(s)` shape (borrow admits, consume/escape excludes).
        //    Runtime borrowing receivers (`(a+b).len()` = `hew_string_length`)
        //    are deliberately excluded: their nested temp already gets an
        //    exactly-once inline release from `apply_nested_fresh_string_temp_drops`.
        self.finalize_borrowed_argument_owners(
            callee_symbol,
            hir_args,
            &arg_places,
            &proven_borrow_args,
        );
        if !proven_borrow_args.is_empty() {
            self.proven_borrow_call_args
                .insert(self.current_block_id, proven_borrow_args);
        }
        if !pending_affine_consumes.is_empty() {
            let args = pending_affine_consumes
                .into_iter()
                .map(|candidate| PendingAffineCallConsumeArg {
                    index: candidate.index,
                    binding: candidate.binding,
                    source: arg_places[candidate.index],
                    guard: candidate.guard,
                    site: candidate.site,
                })
                .collect();
            let replaced = self
                .pending_affine_call_consumes
                .insert(self.current_block_id, PendingAffineCallConsumeSite { args });
            debug_assert!(replaced.is_none(), "one call terminator per basic block");
        }
        self.note_owned_call_site(callee_item, hir_args, &arg_places);
        let authority = if matches!(ret_ty, ResolvedTy::Never) {
            authority.with_no_return()
        } else {
            authority
        };
        self.finish_current_block(Terminator::Call {
            callee: callee_symbol.to_string(),
            authority,
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
            // `seal_body_blocks` seals rather than drops it if it ends up
            // empty at true function end (hew-lang/hew#2425) — see that
            // field's doc comment for the full mechanism.
            self.dead_cursor_is_call_continuation = true;
        } else {
            self.start_block(next);
        }

        dest
    }

    /// Reject a foreign result that can carry a callable returning `string`.
    ///
    /// Hew cannot manufacture an owned-return contract for an opaque callable
    /// pair, including one nested in an aggregate returned by the extern.
    fn reject_opaque_foreign_callable_result(
        &mut self,
        callee_symbol: &str,
        ret_ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> bool {
        let concrete_ret_ty = self.subst_ty(ret_ty);
        if !self
            .call_scrutinee_provenance
            .extern_table
            .is_extern_name(callee_symbol)
            || !crate::model::ty_contains_string_returning_callable(
                &concrete_ret_ty,
                &self.record_layouts_for_classification(),
                &self.enum_layouts,
            )
        {
            return false;
        }
        self.diagnostics.push(MirDiagnostic {
            kind: MirDiagnosticKind::NotYetImplemented {
                construct: format!(
                    "ownership-opaque extern `{callee_symbol}` returning a \
                     string-returning callable value"
                ),
                site,
            },
            note: "a foreign callable pair has no Hew-owned return-share \
                   contract; admitting it directly or through a tuple, record, \
                   enum, or generic container could manufacture a caller \
                   `hew_string_drop` when the callable is later invoked"
                .to_string(),
        });
        true
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
}

/// Materialise a catalogued runtime authority from a checked collection
/// verdict.  The linker spelling is an input only: it must agree with the
/// *operation* selected by the type checker, so `Vec::push` can never mint a
/// `Vec::pop` ABI merely by supplying that symbol.
fn runtime_authority_for_collection(
    target: hew_types::MethodTargetFamily,
    callee: &str,
) -> Option<hew_types::runtime_call::RuntimeCallFamily> {
    use hew_types::{
        runtime_call::{RuntimeCallFamily as Rt, VecScalarOp},
        VecMethod as VecOp,
    };
    use hew_types::{HashMapMethod as Map, HashSetMethod as Set, MethodTargetFamily as Family};

    let runtime = Rt::from_c_symbol(callee)?;
    let allowed = match target {
        Family::HashMap(Map::Insert) => matches!(runtime, Rt::HashMapInsertLayout),
        Family::HashMap(Map::Get) => matches!(runtime, Rt::HashMapGetLayout),
        Family::HashMap(Map::ContainsKey) => matches!(runtime, Rt::HashMapContainsKeyLayout),
        Family::HashMap(Map::Remove) => matches!(runtime, Rt::HashMapRemoveLayout),
        Family::HashMap(Map::Len) => matches!(runtime, Rt::HashMapLenLayout),
        Family::HashMap(Map::Keys) => matches!(runtime, Rt::HashMapKeysLayout),
        Family::HashMap(Map::Values) => matches!(runtime, Rt::HashMapValuesLayout),
        Family::HashMap(Map::Entries) => matches!(runtime, Rt::HashMapEntriesLayout),
        Family::HashMap(Map::Clone) => matches!(runtime, Rt::HashMapCloneLayout),
        Family::HashMap(Map::Clear) => matches!(runtime, Rt::HashMapClearLayout),
        Family::HashSet(Set::Insert) => matches!(runtime, Rt::HashSetInsertLayout),
        Family::HashSet(Set::Contains) => matches!(runtime, Rt::HashSetContainsLayout),
        Family::HashSet(Set::Remove) => matches!(runtime, Rt::HashSetRemoveLayout),
        Family::HashSet(Set::Len) => matches!(runtime, Rt::HashSetLenLayout),
        Family::HashSet(Set::IsEmpty) => matches!(runtime, Rt::HashSetIsEmptyLayout),
        Family::HashSet(Set::Clone) => matches!(runtime, Rt::HashSetCloneLayout),
        Family::HashSet(Set::ToVec) => matches!(runtime, Rt::HashSetToVecLayout),
        Family::HashSet(Set::Clear) => matches!(runtime, Rt::HashSetClearLayout),
        Family::Vec(VecOp::Push) => matches!(
            runtime,
            Rt::VecPushBool
                | Rt::VecPushLayout
                | Rt::VecPushOwned
                | Rt::VecPushOwnedMove
                | Rt::VecScalar {
                    op: VecScalarOp::Push,
                    ..
                }
        ),
        Family::Vec(VecOp::Pop) => {
            matches!(
                runtime,
                Rt::VecPopBool
                    | Rt::VecPopLayout
                    | Rt::VecPopOwned
                    | Rt::VecScalar {
                        op: VecScalarOp::Pop,
                        ..
                    }
            )
        }
        Family::Vec(VecOp::Len) => matches!(runtime, Rt::VecLen),
        Family::Vec(VecOp::IsEmpty) => matches!(runtime, Rt::VecIsEmpty),
        Family::Vec(VecOp::Get) => matches!(runtime, Rt::VecGet(_)),
        Family::Vec(VecOp::Set) => matches!(
            runtime,
            Rt::VecSetBool
                | Rt::VecSetLayout
                | Rt::VecSetOwned
                | Rt::VecSetOwnedMove
                | Rt::VecScalar {
                    op: VecScalarOp::Set,
                    ..
                }
        ),
        Family::Vec(VecOp::Remove) => matches!(
            runtime,
            Rt::VecRemoveAtBool
                | Rt::VecRemoveAtLayout
                | Rt::VecRemoveAtOwned
                | Rt::VecScalar {
                    op: VecScalarOp::RemoveAt,
                    ..
                }
        ),
        Family::Vec(VecOp::Contains) => {
            matches!(
                runtime,
                Rt::VecContainsLayout | Rt::VecContainsOwned | Rt::VecContainsScalar(_)
            )
        }
        Family::Vec(VecOp::Clone) => matches!(
            runtime,
            Rt::VecClone | Rt::VecCloneLayout | Rt::VecCloneOwned
        ),
        Family::Vec(VecOp::Clear) => matches!(runtime, Rt::VecClear),
        Family::Vec(VecOp::Append) => matches!(runtime, Rt::VecAppend),
        Family::Vec(VecOp::Join) => matches!(runtime, Rt::VecJoinStr),
    };
    allowed.then_some(runtime)
}

/// Lift the closure-pair Vec special ABI from the checked Vec operation plus
/// the substituted element representation.  The linker spelling cannot mint
/// this authority on its own: it is merely verified against the exact kind.
fn closure_pair_vec_kind(
    target: hew_types::MethodTargetFamily,
    callee: &str,
    receiver_ty: &ResolvedTy,
) -> Option<crate::ClosurePairVecKind> {
    let ResolvedTy::Named {
        builtin: Some(hew_types::BuiltinType::Vec),
        args,
        ..
    } = receiver_ty
    else {
        return None;
    };
    if !matches!(
        args.first(),
        Some(ResolvedTy::Function { .. } | ResolvedTy::Closure { .. })
    ) {
        return None;
    }
    match (target, callee) {
        (hew_types::MethodTargetFamily::Vec(hew_types::VecMethod::Push), "hew_vec_push_ptr") => {
            Some(crate::ClosurePairVecKind::Push)
        }
        (hew_types::MethodTargetFamily::Vec(hew_types::VecMethod::Set), "hew_vec_set_ptr") => {
            Some(crate::ClosurePairVecKind::Set)
        }
        (hew_types::MethodTargetFamily::Vec(hew_types::VecMethod::Get), "hew_vec_get_ptr") => {
            Some(crate::ClosurePairVecKind::Get)
        }
        (hew_types::MethodTargetFamily::Vec(hew_types::VecMethod::Pop), "hew_vec_pop_ptr") => {
            Some(crate::ClosurePairVecKind::Pop)
        }
        (
            hew_types::MethodTargetFamily::Vec(hew_types::VecMethod::Remove),
            "hew_vec_remove_at_ptr",
        ) => Some(crate::ClosurePairVecKind::RemoveAt),
        _ => None,
    }
}

mod metrics_runtime_calls;
mod vec_element_release;

#[cfg(test)]
mod binding_ty_is_plain_vec_tuple;

// Split into a sibling file (not inlined here) to stay under the
// `src/lower/` line-count ratchet (`hew-mir/tests/lower_module_size.rs`).
#[cfg(test)]
mod poisoned_assign_target_cascade;
