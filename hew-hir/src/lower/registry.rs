//! Function, extern and stdlib registry seeding.

use super::*;

impl LowerCtx {
    pub(super) fn seed_stdlib_fn_registry(&mut self) {
        for (index, builtin) in stdlib_catalog::entries().iter().enumerate() {
            // W4.001 Stage C0b: `LayoutDescriptorSymbol` rows declare
            // `#[no_mangle] pub static` descriptors in `hew-runtime/src/
            // layout_intrinsics.rs`, not callable functions. Skip them
            // here so a user-written `hew_layout_key_i32()` is rejected at
            // HIR resolution (no fn_registry entry) rather than silently
            // resolving and disappearing at MIR (where the descriptor
            // symbol is correctly absent from `module_fn_names`). The
            // catalog rows themselves stay intact for ABI enumeration +
            // `stdlib_catalog_layout_descriptor_coverage`.
            if matches!(
                builtin.linkage,
                stdlib_catalog::BuiltinLinkage::LayoutDescriptorSymbol { .. }
            ) {
                continue;
            }
            // Keep catalog IDs out of the source-item sequence so existing HIR
            // item IDs remain stable while builtin callees still carry Item refs.
            let index = u32::try_from(index).expect("stdlib catalog fits in u32");
            let id = ItemId(u32::MAX - index);
            let param_tys = builtin.params.iter().map(|ty| ty.to_resolved()).collect();
            let return_ty = builtin.return_ty.to_resolved();
            let entry = FnEntry {
                id,
                return_ty,
                param_tys,
                linkage: Some(builtin.linkage),
                type_params: Vec::new(),
                builtin_family: None,
            };
            // Catalog rows whose NAME is itself a codegen `Terminator::Call`
            // intercept identity resolve to `ResolvedRef::Builtin(family)`
            // when their endpoint has a closed runtime family.  Other catalog
            // rows remain catalog-builtin endpoints (for example print
            // intercepts), which are carried through `CallTarget::Builtin`.
            // No admitted catalog entry may fall through to an unsupported
            // synthetic call target merely because it is not a runtime family.
            //
            // The lift must NOT apply to the `runtime_symbol()` alias
            // insert below: the alias callee name (e.g.
            // `hew_node_api_lookup_location`) is a different callee identity than
            // the family's `c_symbol()`, and carrying the family there
            // would break the `Terminator::Call` builtin↔callee invariant.
            let primary_family =
                hew_types::runtime_call::RuntimeCallFamily::from_catalog_endpoint(builtin.name)
                    .or_else(|| {
                        hew_types::runtime_call::RuntimeCallFamily::from_c_symbol(builtin.name)
                    })
                    .or_else(|| {
                        builtin
                            .linkage
                            .runtime_symbol()
                            .and_then(hew_types::runtime_call::RuntimeCallFamily::from_c_symbol)
                    });
            self.fn_registry.insert(
                builtin.name.to_string(),
                FnEntry {
                    builtin_family: primary_family,
                    ..entry.clone()
                },
            );
            if let Some(symbol) = builtin.linkage.runtime_symbol() {
                self.fn_registry.insert(symbol.to_string(), entry);
            }
        }
        self.seed_typed_builtin_fn_registry();
        self.seed_channel_recv_fn_registry();
    }

    /// Seed the `link_remote(RemotePid<T>, PartitionPolicy) -> Result<(),
    /// LinkError>` builtin. Unlike `link`/`monitor`/`unlink` (1-arg
    /// actor handle, self synthesized), `link_remote` is 2-arg: the explicit remote
    /// target and the `PartitionPolicy`. The linking subject (self) is resolved
    /// inside the runtime. The checker records the call-result type at the call
    /// site, so `return_ty` is a placeholder; the params carry arity. Resolves to
    /// `ResolvedRef::Builtin(LinkRemote)` → C symbol `hew_node_link_remote_location`.
    pub(super) fn seed_link_remote_fn_registry(&mut self) {
        use hew_types::runtime_call::RuntimeCallFamily;
        self.fn_registry.insert(
            "link_remote".to_string(),
            FnEntry {
                id: SYNTHETIC_LINK_REMOTE_ITEM,
                return_ty: ResolvedTy::Unit,
                param_tys: vec![
                    ResolvedTy::Named {
                        args: vec![ResolvedTy::Unit],
                        head: hew_types::TypeHead::Builtin(hew_types::BuiltinType::RemotePid),
                        is_opaque: false,
                    },
                    // PartitionPolicy is the stdlib enum the checker also names.
                    ResolvedTy::Named {
                        head: hew_types::KnownDecl::PartitionPolicy.head(),
                        args: vec![],
                        is_opaque: false,
                    },
                ],
                linkage: None,
                type_params: vec!["T".to_string()],
                builtin_family: Some(RuntimeCallFamily::LinkRemote),
            },
        );
    }

    /// Seeds the checker-registered runtime builtins that have no
    /// `stdlib_catalog` entry and no AST `fn` item (`supervisor_stop`,
    /// `link`, `monitor`, `unlink`, `link_remote`). See the per-entry comments for the
    /// resolution contracts.
    pub(super) fn seed_typed_builtin_fn_registry(&mut self) {
        use hew_types::runtime_call::RuntimeCallFamily;
        // Checker-registered runtime-builtin functions that have no stdlib_catalog
        // entry and no AST `fn` item. `builtin_family: Some(...)` makes
        // `lower_identifier` resolve them to `ResolvedRef::Builtin(family)`;
        // MIR's `runtime_symbol_for_call_expr` reads `family.c_symbol()`
        // directly off the catalog bijection. The `id` is a registry
        // placeholder only (the `u32::MAX / 2` band keeps it clear of both
        // stdlib catalog IDs and the source-item sequence) — nothing
        // resolves through it anymore.
        //
        // `supervisor_stop(sup: S) -> ()`, where `S` is the supervisor's own
        // type. This seed carries arity only: MIR passes the sup place
        // opaquely, so the entry names the handle discriminator and nothing
        // resolves the supervisor from it.
        self.fn_registry.insert(
            "supervisor_stop".to_string(),
            FnEntry {
                id: ItemId(u32::MAX / 2),
                return_ty: ResolvedTy::Unit,
                param_tys: vec![ResolvedTy::named_actor_path(
                    &self.defs,
                    hew_types::BuiltinType::ActorHandle.canonical_name(),
                    vec![ResolvedTy::Unit],
                )],
                linkage: None,
                type_params: Vec::new(),
                builtin_family: Some(RuntimeCallFamily::SupervisorStop),
            },
        );
        // Actor `link(target)` / `monitor(target)` / `unlink(target)`
        // builtins.  The checker (`Checker::register_builtins`) registers
        // them as **1-arg** actor-handle — the linking/monitoring subject
        // is the implicit calling actor (`self`), matching Erlang/OTP
        // `link(Pid)` / `monitor(process, Pid)`.  They have no AST `fn`
        // item; `builtin_family` resolves them to `ResolvedRef::Builtin`,
        // and MIR's runtime-call producer synthesizes `hew_actor_self()`
        // as ABI arg0 with the user target as arg1.  The exact inner
        // actor-handle arg matters only for arity; MIR passes the target
        // place opaquely.
        for (name, id, family) in [
            ("link", SYNTHETIC_LINK_ITEM, RuntimeCallFamily::ActorLink),
            (
                "monitor",
                SYNTHETIC_MONITOR_ITEM,
                RuntimeCallFamily::ActorMonitor,
            ),
            (
                "unlink",
                SYNTHETIC_UNLINK_ITEM,
                RuntimeCallFamily::ActorUnlink,
            ),
        ] {
            self.fn_registry.insert(
                name.to_string(),
                FnEntry {
                    id,
                    return_ty: ResolvedTy::Unit,
                    param_tys: vec![ResolvedTy::named_actor_path(
                        &self.defs,
                        hew_types::BuiltinType::ActorHandle.canonical_name(),
                        vec![ResolvedTy::Unit],
                    )],
                    linkage: None,
                    type_params: Vec::new(),
                    builtin_family: Some(family),
                },
            );
        }
        self.seed_link_remote_fn_registry();
        // `instant::now() -> instant`. The static (no-receiver) call resolves
        // by the joined callee name `"instant::now"`; `builtin_family` makes
        // `lower_identifier` produce `ResolvedRef::Builtin(InstantNow)` so MIR
        // reads `hew_instant_now` off the catalog bijection. `instant` is
        // i64-backed, so the entry returns `I64`. No params (the runtime symbol
        // reads the monotonic clock).
        self.fn_registry.insert(
            "instant::now".to_string(),
            FnEntry {
                id: SYNTHETIC_INSTANT_NOW_ITEM,
                return_ty: ResolvedTy::I64,
                param_tys: Vec::new(),
                linkage: None,
                type_params: Vec::new(),
                builtin_family: Some(RuntimeCallFamily::InstantNow),
            },
        );
    }

    /// Seeds `fn_registry` entries for the pipe layout-witness recv/send
    /// symbols (`hew_stream_next_layout`, `hew_stream_try_next_layout`,
    /// `hew_stream_send_layout`, `hew_stream_try_send_layout`).
    ///
    /// These carry an out-parameter and/or element-layout-witness ABI and are
    /// not extern-declarable in `.hew` source. Return types are placeholders
    /// (MIR reads the actual `Option<T>` type from `expr_types` at the call
    /// site); param types carry only arity. See
    /// `SYNTHETIC_STREAM_NEXT_LAYOUT_ITEM` for the full rationale.
    pub(super) fn seed_channel_recv_fn_registry(&mut self) {
        for (name, id) in [
            ("hew_stream_next_layout", SYNTHETIC_STREAM_NEXT_LAYOUT_ITEM),
            (
                "hew_stream_try_next_layout",
                SYNTHETIC_STREAM_TRY_NEXT_LAYOUT_ITEM,
            ),
            ("hew_stream_send_layout", SYNTHETIC_STREAM_SEND_LAYOUT_ITEM),
            (
                "hew_stream_try_send_layout",
                SYNTHETIC_STREAM_TRY_SEND_LAYOUT_ITEM,
            ),
        ] {
            // The registry name IS the catalog `c_symbol`, so the bijection
            // lift cannot fail. Carrying the family here covers the stdlib
            // impl bodies (`std/channel/channel.hew` calls these entries as
            // bare identifiers inside `unsafe {}`) — the call inside the
            // body must reach codegen's family-keyed `Terminator::Call`
            // intercept exactly like a checker-rewritten call site does.
            let builtin_family = hew_types::runtime_call::RuntimeCallFamily::from_c_symbol(name);
            debug_assert!(
                builtin_family.is_some(),
                "layout entry `{name}` not in catalog"
            );
            self.fn_registry.insert(
                name.to_string(),
                FnEntry {
                    id,
                    return_ty: ResolvedTy::Unit,
                    param_tys: Vec::new(),
                    linkage: None,
                    type_params: Vec::new(),
                    builtin_family,
                },
            );
        }
    }

    pub(super) fn register_fn_entry(&mut self, name: &str, func: &FnDecl) -> ItemId {
        // A later ordinary function registration must not inherit a stale
        // extern privilege merely by reusing its symbol spelling.
        self.extern_fn_names.remove(name);
        let id = self.ids.item();
        let return_ty = func
            .return_type
            .as_ref()
            .map_or(ResolvedTy::Unit, |ty| self.lower_type(ty));
        let param_tys = func.params.iter().map(|p| self.lower_type(&p.ty)).collect();
        let type_params = func
            .type_params
            .as_ref()
            .map(|params| params.iter().map(|p| p.name.to_string()).collect())
            .unwrap_or_default();
        self.fn_registry.insert(
            name.to_string(),
            FnEntry {
                id,
                return_ty,
                param_tys,
                linkage: None,
                type_params,
                builtin_family: None,
            },
        );
        id
    }

    pub(super) fn register_impl_method_fn_entry(
        &mut self,
        self_type_name: &str,
        method: &FnDecl,
        impl_type_params: &[String],
    ) {
        let bare_type_name = Self::bare_impl_self_type_name(self_type_name);
        let symbol =
            crate::node::HirImplBlock::method_symbol(self_type_name, method.name.name.as_str());
        self.register_fn_entry(&symbol, method);
        if Self::is_var_self_method_for_type(method, Some(bare_type_name)) {
            if let Some(entry) = self.fn_registry.get_mut(&symbol) {
                let Some(receiver_ty) = entry.param_tys.first().cloned() else {
                    unreachable!(
                        "var-self method `{symbol}` was registered without a receiver parameter"
                    );
                };
                entry.return_ty =
                    Self::var_self_dual_return_ty(entry.return_ty.clone(), receiver_ty);
            }
        }
        if impl_type_params.is_empty() {
            return;
        }
        if let Some(entry) = self.fn_registry.get_mut(&symbol) {
            let method_type_params = std::mem::take(&mut entry.type_params);
            entry
                .type_params
                .reserve(impl_type_params.len() + method_type_params.len());
            entry.type_params.extend(impl_type_params.iter().cloned());
            entry.type_params.extend(method_type_params);
        }
    }

    /// Register an `extern "..." { fn ...; }` declaration in `fn_registry`
    /// so call sites resolve the bare name to an item just like a regular
    /// `fn`. Extern fns are monomorphic (no type params) and have no body
    /// — codegen pre-declares the LLVM symbol with external linkage.
    pub(super) fn register_extern_fn_entry(&mut self, decl: &hew_parser::ast::ExternFnDecl) {
        let id = self.ids.item();
        let return_ty = decl
            .return_type
            .as_ref()
            .map_or(ResolvedTy::Unit, |ty| self.lower_type(ty));
        let param_tys = decl.params.iter().map(|p| self.lower_type(&p.ty)).collect();
        self.fn_registry.insert(
            decl.name.to_string(),
            FnEntry {
                id,
                return_ty,
                param_tys,
                linkage: None,
                type_params: Vec::new(),
                builtin_family: None,
            },
        );
        self.extern_fn_names.insert(decl.name.to_string());
    }
}
