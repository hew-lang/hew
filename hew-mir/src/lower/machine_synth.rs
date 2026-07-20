use super::{
    build_down_hook_body, build_exit_hook_body, crash_action_return_ty, is_crash_info_payload_ty,
    lower_function, resource_drop_fn, ActorHandlerLayout, ActorLayout, BindingId, BlockKind,
    Builder, CheckedMirFunction, ChildSlot, CmpPred, ElabBlock, ElaboratedMirFunction, HashMap,
    HashSet, HirActorDecl, HirBinding, HirBlock, HirExpr, HirExprKind, HirFn, HirLifecycleHookKind,
    HirMachineDecl, HirMachineTransition, HirNodeId, HirStmt, HirStmtKind, HirSupervisorChild,
    HirSupervisorDecl, Instr, IntentKind, LoweredFunction, MirDiagnostic, MirDiagnosticKind,
    ParamOwnershipFacts, Place, PointerWidth, RawMirFunction, Rc, ResolvedRef, ResolvedTy, ScopeId,
    SiteId, SourceOrigin, TaskEntryAdapterSymbols, Terminator, ThirFunction, TrapKind, ValueClass,
    SENTINEL_CRASH_CODE_BINDING, SENTINEL_CRASH_CODE_NODE, SENTINEL_CRASH_CODE_SITE,
    SENTINEL_CRASH_MESSAGE_BINDING, SENTINEL_DOWN_CRASH_KIND_BINDING,
    SENTINEL_DOWN_LOCAL_SLOT_BINDING, SENTINEL_DOWN_LOCATION_BINDING,
    SENTINEL_DOWN_MONITOR_ID_BINDING, SENTINEL_DOWN_REASON_KIND_BINDING,
    SENTINEL_DOWN_TARGET_KIND_BINDING, SENTINEL_EXIT_ACTOR_ID_BINDING,
    SENTINEL_EXIT_KIND_TAG_BINDING,
};
use crate::model::ActorHandlerKind;

fn with_actor_handler_identity(
    mut lowered: LoweredFunction,
    actor: &HirActorDecl,
    kind: ActorHandlerKind,
) -> LoweredFunction {
    lowered.raw.source_origin = SourceOrigin::SynthesizedActorHandler {
        kind,
        actor_layout_key: actor.qualified_name(),
    };
    lowered
}

/// Build the synthetic `HirFn` params + return type for a `receive fn`
/// handler shell. For an ordinary handler this is just `(handler.params,
/// handler.return_ty)`, unchanged.
///
/// For a generator handler (`receive gen fn`), the HIR-lowered body is a thin
/// block whose tail is a `HirExprKind::GenBlock` typed `Generator<Yield,
/// Unit>` (see `lower_actor_generator_body` in hew-hir). Lowering with
/// `is_generator: true` routes the body through the `GenBlock` →
/// `Terminator::Yield` state-machine path (`lower_gen_block`), same as a
/// standalone `gen fn`. Unlike a standalone generator, this shell does NOT
/// return the generator handle to a caller: `lower_function` detects
/// `is_generator && FunctionCallConv::ActorHandler` and reshapes it into a
/// stream-producer PUMP (`Builder::build_stream_producer_pump`) that drives
/// the handle to completion, forwarding each yielded value into a sink — so
/// the shell's *MIR* return type is `Unit`, not the generator handle type.
///
/// The pump needs a destination for the forwarded values: one trailing
/// pointer-word sink param, appended after the handler's own params
/// (`ActorHandlerLayout.param_tys` mirrors this exactly — see
/// `lower_actor_handler_layouts`). The sink param carries a synthetic
/// `BindingId` — no HIR `BindingRef` in the (real) handler body ever names
/// it; `lower_function` locates it positionally (`func.params.len() - 1`),
/// not by id.
fn stream_producer_shell_params_and_return_ty(
    handler: &hew_hir::HirActorReceiveFn,
) -> (Vec<HirBinding>, ResolvedTy) {
    if !handler.is_generator {
        return (handler.params.clone(), handler.return_ty.clone());
    }
    let mut params = handler.params.clone();
    let sink_ty = ResolvedTy::named_builtin(
        "Sink",
        hew_types::BuiltinType::Sink,
        vec![handler.return_ty.clone()],
    );
    params.push(HirBinding {
        id: BindingId(u32::MAX),
        name: "__hew_gen_sink".to_string(),
        ty: sink_ty,
        mutable: false,
        span: handler.span.clone(),
        is_consume: false,
    });
    (params, ResolvedTy::Unit)
}
#[allow(
    clippy::too_many_arguments,
    reason = "actor receive lowering threads the same module tables as regular function lowering"
)]
fn lower_actor_receive_handlers(
    actor: &HirActorDecl,
    type_classes: &hew_hir::TypeClassTable,
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    actor_layouts: &HashMap<String, ActorLayout>,
    machine_layout_names: &HashSet<String>,
    enum_layouts: &[crate::model::EnumLayout],
    opaque_handle_names: &[String],
    module_fn_names: &HashSet<String>,
    module_generic_fn_names: &HashSet<String>,
    funcupdate_fn_returns_fresh: &Rc<HashMap<hew_hir::ItemId, bool>>,
    call_scrutinee_provenance: &Rc<crate::return_provenance::CallScrutineeProvenance>,
    param_ownership: &Rc<ParamOwnershipFacts>,
    call_site_type_args: &HashMap<hew_hir::SiteId, Vec<ResolvedTy>>,
    supervisor_child_slots: &HashMap<hew_hir::SiteId, ChildSlot>,
    pool_accessor_sites: &HashMap<hew_hir::SiteId, hew_types::PoolAccessor>,
    actor_send_aliasing: &HashMap<hew_types::SpanKey, hew_types::ActorSendAliasing>,
    pointer_width: PointerWidth,
    emitted_symbols: &mut HashMap<String, String>,
    task_entry_adapter_symbols: &TaskEntryAdapterSymbols,
    diagnostics: &mut Vec<MirDiagnostic>,
) -> Vec<LoweredFunction> {
    let state_fields: HashSet<String> = actor
        .state_fields
        .iter()
        .map(|field| field.name.clone())
        .collect();
    let mut lowered = Vec::new();

    for handler in &actor.receive_handlers {
        let self_field_errors = unknown_self_fields_in_block(&handler.body, &state_fields);
        if !self_field_errors.is_empty() {
            for field in self_field_errors {
                diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::UnknownActorStateField {
                        actor: actor.name.clone(),
                        field: field.clone(),
                    },
                    note: format!(
                        "actor `{}` receive fn `{}` references unknown state field `self.{field}`",
                        actor.name, handler.name
                    ),
                });
            }
            continue;
        }

        let emit_name = mangle_actor_receive_handler(&actor_symbol_base(actor), &handler.name);
        let duplicate_label = format!("actor `{}` receive fn `{}`", actor.name, handler.name);
        if let Some(existing) = emitted_symbols.get(&emit_name) {
            diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::ActorHandlerSymbolCollision {
                    symbol: emit_name,
                    existing: existing.clone(),
                    duplicate: duplicate_label,
                },
                note:
                    "actor receive handler symbol mangling must be one-to-one before MIR emission"
                        .to_string(),
            });
            continue;
        }
        emitted_symbols.insert(emit_name.clone(), duplicate_label);

        // A generator handler (`receive gen fn`) lowers its `GenBlock`-tailed
        // body the same way a standalone `gen fn` does, but `lower_function`
        // reshapes the shell into a stream-producer PUMP (see
        // `stream_producer_shell_params_and_return_ty`) rather than returning
        // the generator handle.
        let (params, fn_return_ty) = stream_producer_shell_params_and_return_ty(handler);
        let synthetic_fn = HirFn {
            id: actor.id,
            node: actor.node,
            name: format!("{}::{}", actor.name, handler.name),
            type_params: Vec::new(),
            params,
            return_ty: fn_return_ty,
            body: handler.body.clone(),
            span: handler.span.clone(),
            is_generator: handler.is_generator,
            intrinsic_id: None,
        };
        lowered.push(with_actor_handler_identity(
            lower_function(
                &synthetic_fn,
                emit_name,
                HashMap::new(),
                type_classes,
                record_field_orders,
                actor_layouts,
                &HashMap::new(),
                machine_layout_names,
                enum_layouts,
                opaque_handle_names,
                Some(actor.qualified_name().as_str()),
                module_fn_names,
                module_generic_fn_names,
                funcupdate_fn_returns_fresh,
                call_scrutinee_provenance,
                param_ownership,
                &HashMap::new(),
                call_site_type_args,
                None,
                supervisor_child_slots,
                pool_accessor_sites,
                actor_send_aliasing,
                pointer_width,
                crate::model::FunctionCallConv::ActorHandler,
                task_entry_adapter_symbols.clone(),
            ),
            actor,
            ActorHandlerKind::Receive,
        ));
    }

    lowered
}
#[allow(
    clippy::too_many_arguments,
    reason = "actor body lowering threads the same module tables as regular function lowering"
)]
pub(super) fn lower_actor_body_handlers(
    actor: &HirActorDecl,
    type_classes: &hew_hir::TypeClassTable,
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    actor_layouts: &HashMap<String, ActorLayout>,
    machine_layout_names: &HashSet<String>,
    enum_layouts: &[crate::model::EnumLayout],
    opaque_handle_names: &[String],
    module_fn_names: &HashSet<String>,
    module_generic_fn_names: &HashSet<String>,
    funcupdate_fn_returns_fresh: &Rc<HashMap<hew_hir::ItemId, bool>>,
    call_scrutinee_provenance: &Rc<crate::return_provenance::CallScrutineeProvenance>,
    param_ownership: &Rc<ParamOwnershipFacts>,
    call_site_type_args: &HashMap<hew_hir::SiteId, Vec<ResolvedTy>>,
    supervisor_child_slots: &HashMap<hew_hir::SiteId, ChildSlot>,
    pool_accessor_sites: &HashMap<hew_hir::SiteId, hew_types::PoolAccessor>,
    actor_send_aliasing: &HashMap<hew_types::SpanKey, hew_types::ActorSendAliasing>,
    pointer_width: PointerWidth,
    emitted_symbols: &mut HashMap<String, String>,
    task_entry_adapter_symbols: &TaskEntryAdapterSymbols,
    diagnostics: &mut Vec<MirDiagnostic>,
) -> Vec<LoweredFunction> {
    let mut lowered = Vec::new();
    if let Some(init) = &actor.init {
        if let Some(func) = lower_actor_init_handler(
            actor,
            init,
            type_classes,
            record_field_orders,
            actor_layouts,
            machine_layout_names,
            enum_layouts,
            opaque_handle_names,
            module_fn_names,
            module_generic_fn_names,
            funcupdate_fn_returns_fresh,
            call_scrutinee_provenance,
            param_ownership,
            call_site_type_args,
            supervisor_child_slots,
            pool_accessor_sites,
            actor_send_aliasing,
            pointer_width,
            emitted_symbols,
            task_entry_adapter_symbols,
            diagnostics,
        ) {
            lowered.push(func);
        }
    }
    lowered.extend(lower_actor_lifecycle_handlers(
        actor,
        type_classes,
        record_field_orders,
        actor_layouts,
        machine_layout_names,
        enum_layouts,
        opaque_handle_names,
        module_fn_names,
        module_generic_fn_names,
        funcupdate_fn_returns_fresh,
        call_scrutinee_provenance,
        param_ownership,
        call_site_type_args,
        supervisor_child_slots,
        pool_accessor_sites,
        actor_send_aliasing,
        pointer_width,
        emitted_symbols,
        task_entry_adapter_symbols,
        diagnostics,
    ));
    lowered.extend(lower_actor_receive_handlers(
        actor,
        type_classes,
        record_field_orders,
        actor_layouts,
        machine_layout_names,
        enum_layouts,
        opaque_handle_names,
        module_fn_names,
        module_generic_fn_names,
        funcupdate_fn_returns_fresh,
        call_scrutinee_provenance,
        param_ownership,
        call_site_type_args,
        supervisor_child_slots,
        pool_accessor_sites,
        actor_send_aliasing,
        pointer_width,
        emitted_symbols,
        task_entry_adapter_symbols,
        diagnostics,
    ));
    lowered
}
#[allow(
    clippy::too_many_arguments,
    reason = "actor init lowering threads the same module tables as regular function lowering"
)]
fn lower_actor_init_handler(
    actor: &HirActorDecl,
    init: &hew_hir::HirActorInit,
    type_classes: &hew_hir::TypeClassTable,
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    actor_layouts: &HashMap<String, ActorLayout>,
    machine_layout_names: &HashSet<String>,
    enum_layouts: &[crate::model::EnumLayout],
    opaque_handle_names: &[String],
    module_fn_names: &HashSet<String>,
    module_generic_fn_names: &HashSet<String>,
    funcupdate_fn_returns_fresh: &Rc<HashMap<hew_hir::ItemId, bool>>,
    call_scrutinee_provenance: &Rc<crate::return_provenance::CallScrutineeProvenance>,
    param_ownership: &Rc<ParamOwnershipFacts>,
    call_site_type_args: &HashMap<hew_hir::SiteId, Vec<ResolvedTy>>,
    supervisor_child_slots: &HashMap<hew_hir::SiteId, ChildSlot>,
    pool_accessor_sites: &HashMap<hew_hir::SiteId, hew_types::PoolAccessor>,
    actor_send_aliasing: &HashMap<hew_types::SpanKey, hew_types::ActorSendAliasing>,
    pointer_width: PointerWidth,
    emitted_symbols: &mut HashMap<String, String>,
    task_entry_adapter_symbols: &TaskEntryAdapterSymbols,
    diagnostics: &mut Vec<MirDiagnostic>,
) -> Option<LoweredFunction> {
    let emit_name = mangle_actor_init_handler(&actor_symbol_base(actor));
    let duplicate_label = format!("actor `{}` init", actor.name);
    if let Some(existing) = emitted_symbols.get(&emit_name) {
        diagnostics.push(MirDiagnostic {
            kind: MirDiagnosticKind::ActorHandlerSymbolCollision {
                symbol: emit_name,
                existing: existing.clone(),
                duplicate: duplicate_label,
            },
            note: "actor init handler symbol mangling must be one-to-one before MIR emission"
                .to_string(),
        });
        return None;
    }
    emitted_symbols.insert(emit_name.clone(), duplicate_label);

    let synthetic_fn = HirFn {
        id: actor.id,
        node: actor.node,
        name: format!("{}::init", actor.name),
        type_params: Vec::new(),
        params: init.params.clone(),
        return_ty: ResolvedTy::Unit,
        body: init.body.clone(),
        span: actor.span.clone(),
        is_generator: false,
        intrinsic_id: None,
    };
    Some(with_actor_handler_identity(
        lower_function(
            &synthetic_fn,
            emit_name,
            HashMap::new(),
            type_classes,
            record_field_orders,
            actor_layouts,
            &HashMap::new(),
            machine_layout_names,
            enum_layouts,
            opaque_handle_names,
            Some(&actor.name),
            module_fn_names,
            module_generic_fn_names,
            funcupdate_fn_returns_fresh,
            call_scrutinee_provenance,
            param_ownership,
            &HashMap::new(),
            call_site_type_args,
            None,
            supervisor_child_slots,
            pool_accessor_sites,
            actor_send_aliasing,
            pointer_width,
            crate::model::FunctionCallConv::ActorHandler,
            task_entry_adapter_symbols.clone(),
        ),
        actor,
        ActorHandlerKind::Init,
    ))
}
#[allow(
    clippy::too_many_arguments,
    reason = "actor lifecycle lowering threads the same module tables as regular function lowering"
)]
#[allow(
    clippy::too_many_lines,
    reason = "each lifecycle hook kind (Start, Stop, Crash, Upgrade) needs its own arm; extracting would not reduce conceptual complexity"
)]
fn lower_actor_lifecycle_handlers(
    actor: &HirActorDecl,
    type_classes: &hew_hir::TypeClassTable,
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    actor_layouts: &HashMap<String, ActorLayout>,
    machine_layout_names: &HashSet<String>,
    enum_layouts: &[crate::model::EnumLayout],
    opaque_handle_names: &[String],
    module_fn_names: &HashSet<String>,
    module_generic_fn_names: &HashSet<String>,
    funcupdate_fn_returns_fresh: &Rc<HashMap<hew_hir::ItemId, bool>>,
    call_scrutinee_provenance: &Rc<crate::return_provenance::CallScrutineeProvenance>,
    param_ownership: &Rc<ParamOwnershipFacts>,
    call_site_type_args: &HashMap<hew_hir::SiteId, Vec<ResolvedTy>>,
    supervisor_child_slots: &HashMap<hew_hir::SiteId, ChildSlot>,
    pool_accessor_sites: &HashMap<hew_hir::SiteId, hew_types::PoolAccessor>,
    actor_send_aliasing: &HashMap<hew_types::SpanKey, hew_types::ActorSendAliasing>,
    pointer_width: PointerWidth,
    emitted_symbols: &mut HashMap<String, String>,
    task_entry_adapter_symbols: &TaskEntryAdapterSymbols,
    diagnostics: &mut Vec<MirDiagnostic>,
) -> Vec<LoweredFunction> {
    let mut lowered = Vec::new();
    for (hook_idx, hook) in actor.lifecycle_hooks.iter().enumerate() {
        match hook.kind {
            HirLifecycleHookKind::Start => {
                let emit_name = mangle_actor_start_handler(&actor_symbol_base(actor));
                let duplicate_label =
                    format!("actor `{}` #[on(start)] hook `{}`", actor.name, hook.name);
                if let Some(existing) = emitted_symbols.get(&emit_name) {
                    diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::ActorHandlerSymbolCollision {
                            symbol: emit_name,
                            existing: existing.clone(),
                            duplicate: duplicate_label,
                        },
                        note: "actor #[on(start)] handler symbol mangling must be one-to-one before MIR emission"
                            .to_string(),
                    });
                    continue;
                }
                emitted_symbols.insert(emit_name.clone(), duplicate_label);

                let synthetic_fn = HirFn {
                    id: actor.id,
                    node: actor.node,
                    name: format!("{}::{}", actor.name, hook.name),
                    type_params: Vec::new(),
                    params: hook.params.clone(),
                    return_ty: hook.return_ty.clone(),
                    body: hook.body.clone(),
                    span: hook.span.clone(),
                    is_generator: false,
                    intrinsic_id: None,
                };
                lowered.push(with_actor_handler_identity(
                    lower_function(
                        &synthetic_fn,
                        emit_name,
                        HashMap::new(),
                        type_classes,
                        record_field_orders,
                        actor_layouts,
                        &HashMap::new(),
                        machine_layout_names,
                        enum_layouts,
                        opaque_handle_names,
                        Some(actor.qualified_name().as_str()),
                        module_fn_names,
                        module_generic_fn_names,
                        funcupdate_fn_returns_fresh,
                        call_scrutinee_provenance,
                        param_ownership,
                        &HashMap::new(),
                        call_site_type_args,
                        None,
                        supervisor_child_slots,
                        pool_accessor_sites,
                        actor_send_aliasing,
                        pointer_width,
                        crate::model::FunctionCallConv::ActorHandler,
                        task_entry_adapter_symbols.clone(),
                    ),
                    actor,
                    ActorHandlerKind::Start,
                ));
            }
            HirLifecycleHookKind::Stop => {
                // Per-hook unique symbol: <Actor>__on_stop__<hook_idx>.
                // hook_idx is the position of this hook in the actor's full
                // lifecycle_hooks vec (not a stop-specific counter), matching
                // the index used when populating ActorLayout.on_stop_symbols.
                // This guarantees no collisions even when multiple #[on(stop)]
                // hooks are declared on the same actor.
                let emit_name =
                    mangle_actor_stop_handler_indexed(&actor_symbol_base(actor), hook_idx);
                let label = format!("actor `{}` #[on(stop)] hook `{}`", actor.name, hook.name);
                emitted_symbols.insert(emit_name.clone(), label);

                let synthetic_fn = HirFn {
                    id: actor.id,
                    node: actor.node,
                    name: format!("{}::{}", actor.name, hook.name),
                    type_params: Vec::new(),
                    params: hook.params.clone(),
                    return_ty: hook.return_ty.clone(),
                    body: hook.body.clone(),
                    span: hook.span.clone(),
                    is_generator: false,
                    intrinsic_id: None,
                };
                lowered.push(with_actor_handler_identity(
                    lower_function(
                        &synthetic_fn,
                        emit_name,
                        HashMap::new(),
                        type_classes,
                        record_field_orders,
                        actor_layouts,
                        &HashMap::new(),
                        machine_layout_names,
                        enum_layouts,
                        opaque_handle_names,
                        Some(actor.qualified_name().as_str()),
                        module_fn_names,
                        module_generic_fn_names,
                        funcupdate_fn_returns_fresh,
                        call_scrutinee_provenance,
                        param_ownership,
                        &HashMap::new(),
                        call_site_type_args,
                        None,
                        supervisor_child_slots,
                        pool_accessor_sites,
                        actor_send_aliasing,
                        pointer_width,
                        crate::model::FunctionCallConv::ActorHandler,
                        task_entry_adapter_symbols.clone(),
                    ),
                    actor,
                    ActorHandlerKind::Stop,
                ));
            }
            HirLifecycleHookKind::Crash => {
                let emit_name = mangle_actor_crash_handler(&actor_symbol_base(actor));
                let duplicate_label =
                    format!("actor `{}` #[on(crash)] hook `{}`", actor.name, hook.name);
                if let Some(existing) = emitted_symbols.get(&emit_name) {
                    diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::ActorHandlerSymbolCollision {
                            symbol: emit_name,
                            existing: existing.clone(),
                            duplicate: duplicate_label,
                        },
                        note: "actor #[on(crash)] handler symbol mangling must be one-to-one before MIR emission"
                            .to_string(),
                    });
                    continue;
                }
                emitted_symbols.insert(emit_name.clone(), duplicate_label);

                // ABI PROLOGUE — crash-info payload parameter injection:
                //
                // The runtime ABI for `HewOnCrashFn` (M-3 reshape) is
                // `(ctx, crash_code: i64, crash_message: *const c_char,
                //  actor_state_ptr) -> i32` — see
                // `hew-runtime/src/internal/types.rs`.
                //
                // User sources declare a single crash-info payload param and
                // access its `code`. To bridge the raw wire values to the
                // user-visible struct, the user-visible crash-info param is
                // replaced in `abi_params` by TWO synthetic ABI params:
                //   - `__crash_code: i64`     (the trap-kind integer)
                //   - `__crash_message: string` (a `ptr` — borrowed Hew string;
                //     the supervisor owns + frees the buffer)
                // and the body gains a synthetic prologue:
                //
                //   let info = <crash-info-type> {
                //       code: __crash_code,
                //       message: __crash_message.clone(),   // hew_string_clone
                //   };
                //
                // The original binding ID for `info` is preserved in the `Let`
                // statement so every `BindingRef { resolved: Binding(info_id) }`
                // in the user body continues to resolve through `binding_locals`.
                // `__crash_message` is a BORROW (the supervisor owns the Hew
                // header-aware buffer and frees it after the call), so M-5 CLONES it
                // into the owned `CrashInfo.message` field (a fresh `+1` owner the
                // function frame drops once) rather than moving the borrow — moving
                // would double-free with the supervisor's release.
                //
                // RETURN (M-4): the synthetic function's logical return type is
                // `CrashAction`; the hook's `CrashAction` tail/explicit-return
                // value flows through the normal enum-return path and codegen
                // returns the 2-byte `{ i8, [1 x i8] }` tagged-union struct by
                // value. The runtime `HewOnCrashFn` mirrors it with a `#[repr(C)]`
                // 2-byte struct and reads field 0 (the tag). A `panic()`-diverging
                // body returns no value (also valid). There is no i32 coercion and
                // no tag-extraction `match` — the M-3 i32 return was removed in M-4.

                // Find the crash-info payload param (if present) and build ABI
                // params. The payload param is replaced with `__crash_code: I64`;
                // every other param passes through unchanged. Classification is
                // marker/shape driven so the user-visible type name is not
                // load-bearing here.
                let crash_info_param: Option<(HirBinding, String)> =
                    hook.params.iter().find_map(|p| {
                        if !is_crash_info_payload_ty(&p.ty, type_classes, record_field_orders) {
                            return None;
                        }
                        let ResolvedTy::Named { name, .. } = &p.ty else {
                            return None;
                        };
                        Some((p.clone(), name.clone()))
                    });

                // The crash-info payload param expands to TWO ABI params:
                // `__crash_code: i64` then `__crash_message: string` (a `ptr`),
                // matching the `HewOnCrashFn` signature `(ctx, i64, *const
                // c_char, *mut c_void) -> i32`. The message param is a borrow
                // (the runtime owns the buffer) and is unused until M-5 seeds
                // `CrashInfo.message` from it. Non-payload params pass through.
                let abi_params: Vec<HirBinding> = hook
                    .params
                    .iter()
                    .flat_map(|p| {
                        if is_crash_info_payload_ty(&p.ty, type_classes, record_field_orders) {
                            vec![
                                HirBinding {
                                    id: SENTINEL_CRASH_CODE_BINDING,
                                    name: "__crash_code".to_string(),
                                    ty: ResolvedTy::I64,
                                    mutable: false,
                                    span: p.span.clone(),
                                    is_consume: false,
                                },
                                HirBinding {
                                    id: SENTINEL_CRASH_MESSAGE_BINDING,
                                    name: "__crash_message".to_string(),
                                    ty: ResolvedTy::String,
                                    mutable: false,
                                    span: p.span.clone(),
                                    is_consume: false,
                                },
                            ]
                        } else {
                            vec![p.clone()]
                        }
                    })
                    .collect();

                // Inject a synthetic `let info = <payload> { code: __crash_code }`
                // at the front of the body when the original signature had a
                // crash-info payload param (the normal `on(crash)` shape).
                let body = if let Some((info_param, crash_info_type_name)) = crash_info_param {
                    let crash_info_ty = info_param.ty.clone();
                    // Build the `__crash_code` BindingRef expression.
                    let crash_code_ref = HirExpr {
                        node: SENTINEL_CRASH_CODE_NODE,
                        site: SENTINEL_CRASH_CODE_SITE,
                        ty: ResolvedTy::I64,
                        value_class: ValueClass::BitCopy,
                        intent: IntentKind::Read,
                        kind: HirExprKind::BindingRef {
                            name: "__crash_code".to_string(),
                            resolved: ResolvedRef::Binding(SENTINEL_CRASH_CODE_BINDING),
                        },
                        span: info_param.span.clone(),
                    };

                    // M-5: build the `__crash_message` BindingRef. The param is a
                    // BORROW — the supervisor owns the underlying Hew header-aware
                    // string (allocated via `str_to_malloc`, rc==1) and frees it via
                    // `free_cstring` after the call (see `invoke_on_crash_handler`).
                    // The field-init must NOT MOVE this borrow into the owned
                    // `CrashInfo.message`: `lower_record_init` raw-stores its field
                    // source with no construction retain, so a move would make the
                    // record a raw rc==1 alias of the supervisor's buffer; the frame's
                    // `CrashInfo` drop would free it (rc->0) and the supervisor's
                    // `free_cstring` would then double-free it — the reported M-5
                    // abort/corruption. Instead CLONE the borrow into an independent
                    // `+1` owner via `RecordCloneCall` (its `string` arm emits
                    // `hew_string_clone`, a header-aware refcount bump). The clone is
                    // MOVED into `CrashInfo.message`, so the record owns its own copy:
                    // rc == supervisor 1 -> clone 2 -> (record owns the clone) ->
                    // `CrashInfo` drop releases it (->1) -> supervisor `free_cstring`
                    // (->0). One buffer, freed exactly once, no double-free.
                    let crash_message_ref = HirExpr {
                        node: SENTINEL_CRASH_CODE_NODE,
                        site: SENTINEL_CRASH_CODE_SITE,
                        ty: ResolvedTy::String,
                        value_class: ValueClass::CowValue,
                        intent: IntentKind::Read,
                        kind: HirExprKind::BindingRef {
                            name: "__crash_message".to_string(),
                            resolved: ResolvedRef::Binding(SENTINEL_CRASH_MESSAGE_BINDING),
                        },
                        span: info_param.span.clone(),
                    };
                    // Clone the owned param into the record's `+1` owner.
                    // `clone_fn_sym` / `record_name` are ignored by the `string` arm
                    // (it emits `hew_string_clone` directly); set for dump/verify.
                    let crash_message_clone = HirExpr {
                        node: SENTINEL_CRASH_CODE_NODE,
                        site: SENTINEL_CRASH_CODE_SITE,
                        ty: ResolvedTy::String,
                        value_class: ValueClass::CowValue,
                        intent: IntentKind::Read,
                        kind: HirExprKind::RecordCloneCall {
                            src: Box::new(crash_message_ref),
                            clone_fn_sym: "hew_string_clone".to_string(),
                            record_name: "string".to_string(),
                        },
                        span: info_param.span.clone(),
                    };

                    // Build `<payload> { code: __crash_code, message: <cloned message> }`.
                    // `CrashInfo` is an owned-aggregate record (`CowValue`) because of
                    // the `message: string` field; it OWNS the cloned message.
                    let struct_init = HirExpr {
                        node: SENTINEL_CRASH_CODE_NODE,
                        site: SENTINEL_CRASH_CODE_SITE,
                        ty: crash_info_ty.clone(),
                        value_class: ValueClass::CowValue,
                        intent: IntentKind::Unknown,
                        kind: HirExprKind::StructInit {
                            name: crash_info_type_name,
                            type_args: Vec::new(),
                            fields: vec![
                                ("code".to_string(), crash_code_ref),
                                ("message".to_string(), crash_message_clone),
                            ],
                            base: None,
                        },
                        span: info_param.span.clone(),
                    };

                    // Build `let info = <payload> { code: __crash_code }`.
                    // Preserve `info_param.id` so user `BindingRef { resolved: Binding(id) }` resolves.
                    let let_info_stmt = HirStmt {
                        node: SENTINEL_CRASH_CODE_NODE,
                        kind: HirStmtKind::Let(
                            HirBinding {
                                id: info_param.id,
                                name: info_param.name.clone(),
                                ty: crash_info_ty,
                                mutable: false,
                                span: info_param.span.clone(),
                                is_consume: false,
                            },
                            Some(struct_init),
                        ),
                        span: info_param.span.clone(),
                    };

                    // Prepend the synthetic let before the original body statements.
                    let mut stmts = Vec::with_capacity(hook.body.statements.len() + 1);
                    stmts.push(let_info_stmt);
                    stmts.extend(hook.body.statements.iter().cloned());
                    HirBlock {
                        statements: stmts,
                        ..hook.body.clone()
                    }
                } else {
                    hook.body.clone()
                };

                // M-4: the hook body returns a `CrashAction` value (or diverges).
                // The synthetic function's logical return type IS `CrashAction`,
                // so EVERY return position — the tail expression AND any explicit
                // `return CrashAction::X;` statement — type-checks and lowers
                // naturally through the existing enum-return path. The
                // codegen-emitted `__on_crash` returns the `CrashAction`
                // tagged-union struct by value; the runtime `HewOnCrashFn` ABI
                // mirrors it with a `#[repr(C)]` 2-byte struct and reads the tag
                // byte. A `panic()`-diverging body returns no value (also valid).
                let synthetic_fn = HirFn {
                    id: actor.id,
                    node: actor.node,
                    name: format!("{}::{}", actor.name, hook.name),
                    type_params: Vec::new(),
                    params: abi_params,
                    return_ty: crash_action_return_ty(),
                    body,
                    span: hook.span.clone(),
                    is_generator: false,
                    intrinsic_id: None,
                };
                lowered.push(with_actor_handler_identity(
                    lower_function(
                        &synthetic_fn,
                        emit_name,
                        HashMap::new(),
                        type_classes,
                        record_field_orders,
                        actor_layouts,
                        &HashMap::new(),
                        machine_layout_names,
                        enum_layouts,
                        opaque_handle_names,
                        Some(actor.qualified_name().as_str()),
                        module_fn_names,
                        module_generic_fn_names,
                        funcupdate_fn_returns_fresh,
                        call_scrutinee_provenance,
                        param_ownership,
                        &HashMap::new(),
                        call_site_type_args,
                        None,
                        supervisor_child_slots,
                        pool_accessor_sites,
                        actor_send_aliasing,
                        pointer_width,
                        crate::model::FunctionCallConv::ActorHandler,
                        task_entry_adapter_symbols.clone(),
                    ),
                    actor,
                    ActorHandlerKind::Crash,
                ));
            }
            HirLifecycleHookKind::Exit => {
                // M-7-R: `#[on(exit)]` linked-actor exit hook. The runtime
                // delivers a peer's `CrashNotification` on SYS_MSG_EXIT as two
                // raw ABI params (`__exit_actor_id: u64`, `__exit_kind_tag: i32`);
                // the prologue rebuilds `note = CrashNotification { actor_id,
                // kind }`. Returns `()` (the hook reacts; it does not steer).
                let emit_name = mangle_actor_exit_handler(&actor_symbol_base(actor));
                let duplicate_label =
                    format!("actor `{}` #[on(exit)] hook `{}`", actor.name, hook.name);
                if let Some(existing) = emitted_symbols.get(&emit_name) {
                    diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::ActorHandlerSymbolCollision {
                            symbol: emit_name,
                            existing: existing.clone(),
                            duplicate: duplicate_label,
                        },
                        note: "actor #[on(exit)] handler symbol mangling must be one-to-one before MIR emission"
                            .to_string(),
                    });
                    continue;
                }
                emitted_symbols.insert(emit_name.clone(), duplicate_label);

                // The CrashNotification payload param (if present) expands to the
                // two raw ABI params; other params pass through (there are none
                // in the canonical shape).
                let notification_param: Option<HirBinding> = hook.params.iter().find_map(|p| {
                    matches!(&p.ty, ResolvedTy::Named { name, args, .. }
                        if name == "CrashNotification" && args.is_empty())
                    .then(|| p.clone())
                });

                let abi_params: Vec<HirBinding> = hook
                    .params
                    .iter()
                    .flat_map(|p| {
                        let is_notification = matches!(&p.ty, ResolvedTy::Named { name, args, .. }
                            if name == "CrashNotification" && args.is_empty());
                        if is_notification {
                            vec![
                                HirBinding {
                                    id: SENTINEL_EXIT_ACTOR_ID_BINDING,
                                    name: "__exit_actor_id".to_string(),
                                    ty: ResolvedTy::U64,
                                    mutable: false,
                                    span: p.span.clone(),
                                    is_consume: false,
                                },
                                HirBinding {
                                    id: SENTINEL_EXIT_KIND_TAG_BINDING,
                                    name: "__exit_kind_tag".to_string(),
                                    ty: ResolvedTy::I32,
                                    mutable: false,
                                    span: p.span.clone(),
                                    is_consume: false,
                                },
                            ]
                        } else {
                            vec![p.clone()]
                        }
                    })
                    .collect();

                let body = if let Some(note_param) = notification_param {
                    build_exit_hook_body(hook.body.clone(), &note_param)
                } else {
                    hook.body.clone()
                };

                let synthetic_fn = HirFn {
                    id: actor.id,
                    node: actor.node,
                    name: format!("{}::{}", actor.name, hook.name),
                    type_params: Vec::new(),
                    params: abi_params,
                    return_ty: ResolvedTy::Unit,
                    body,
                    span: hook.span.clone(),
                    is_generator: false,
                    intrinsic_id: None,
                };
                lowered.push(with_actor_handler_identity(
                    lower_function(
                        &synthetic_fn,
                        emit_name,
                        HashMap::new(),
                        type_classes,
                        record_field_orders,
                        actor_layouts,
                        &HashMap::new(),
                        machine_layout_names,
                        enum_layouts,
                        opaque_handle_names,
                        Some(actor.qualified_name().as_str()),
                        module_fn_names,
                        module_generic_fn_names,
                        funcupdate_fn_returns_fresh,
                        call_scrutinee_provenance,
                        param_ownership,
                        &HashMap::new(),
                        call_site_type_args,
                        None,
                        supervisor_child_slots,
                        pool_accessor_sites,
                        actor_send_aliasing,
                        pointer_width,
                        crate::model::FunctionCallConv::ActorHandler,
                        task_entry_adapter_symbols.clone(),
                    ),
                    actor,
                    ActorHandlerKind::Exit,
                ));
            }
            HirLifecycleHookKind::Down => {
                let emit_name = mangle_actor_down_handler(&actor_symbol_base(actor));
                let duplicate_label =
                    format!("actor `{}` #[on(down)] hook `{}`", actor.name, hook.name);
                if let Some(existing) = emitted_symbols.get(&emit_name) {
                    diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::ActorHandlerSymbolCollision {
                            symbol: emit_name,
                            existing: existing.clone(),
                            duplicate: duplicate_label,
                        },
                        note: "actor #[on(down)] handler symbol mangling must be one-to-one before MIR emission"
                            .to_string(),
                    });
                    continue;
                }
                emitted_symbols.insert(emit_name.clone(), duplicate_label);

                let Some(note_param) = hook.params.first() else {
                    continue;
                };
                let span = note_param.span.clone();
                let abi_params = vec![
                    HirBinding {
                        id: SENTINEL_DOWN_MONITOR_ID_BINDING,
                        name: "__down_monitor_id".to_string(),
                        ty: ResolvedTy::U64,
                        mutable: false,
                        span: span.clone(),
                        is_consume: false,
                    },
                    HirBinding {
                        id: SENTINEL_DOWN_TARGET_KIND_BINDING,
                        name: "__down_target_kind".to_string(),
                        ty: ResolvedTy::I32,
                        mutable: false,
                        span: span.clone(),
                        is_consume: false,
                    },
                    HirBinding {
                        id: SENTINEL_DOWN_REASON_KIND_BINDING,
                        name: "__down_reason_kind".to_string(),
                        ty: ResolvedTy::I32,
                        mutable: false,
                        span: span.clone(),
                        is_consume: false,
                    },
                    HirBinding {
                        id: SENTINEL_DOWN_LOCATION_BINDING,
                        name: "__down_location".to_string(),
                        ty: ResolvedTy::named_builtin(
                            "Location",
                            hew_types::BuiltinType::Location,
                            Vec::new(),
                        ),
                        mutable: false,
                        span: span.clone(),
                        is_consume: false,
                    },
                    HirBinding {
                        id: SENTINEL_DOWN_LOCAL_SLOT_BINDING,
                        name: "__down_local_slot".to_string(),
                        ty: ResolvedTy::U64,
                        mutable: false,
                        span: span.clone(),
                        is_consume: false,
                    },
                    HirBinding {
                        id: SENTINEL_DOWN_CRASH_KIND_BINDING,
                        name: "__down_crash_kind".to_string(),
                        ty: ResolvedTy::I32,
                        mutable: false,
                        span,
                        is_consume: false,
                    },
                ];
                let synthetic_fn = HirFn {
                    id: actor.id,
                    node: actor.node,
                    name: format!("{}::{}", actor.name, hook.name),
                    type_params: Vec::new(),
                    params: abi_params,
                    return_ty: ResolvedTy::Unit,
                    body: build_down_hook_body(hook.body.clone(), note_param),
                    span: hook.span.clone(),
                    is_generator: false,
                    intrinsic_id: None,
                };
                lowered.push(with_actor_handler_identity(
                    lower_function(
                        &synthetic_fn,
                        emit_name,
                        HashMap::new(),
                        type_classes,
                        record_field_orders,
                        actor_layouts,
                        &HashMap::new(),
                        machine_layout_names,
                        enum_layouts,
                        opaque_handle_names,
                        Some(actor.qualified_name().as_str()),
                        module_fn_names,
                        module_generic_fn_names,
                        funcupdate_fn_returns_fresh,
                        call_scrutinee_provenance,
                        param_ownership,
                        &HashMap::new(),
                        call_site_type_args,
                        None,
                        supervisor_child_slots,
                        pool_accessor_sites,
                        actor_send_aliasing,
                        pointer_width,
                        crate::model::FunctionCallConv::ActorHandler,
                        task_entry_adapter_symbols.clone(),
                    ),
                    actor,
                    ActorHandlerKind::Down,
                ));
            }
            HirLifecycleHookKind::Upgrade => push_lifecycle_not_wired_diagnostic(
                diagnostics,
                &actor.name,
                &hook.name,
                "OnUpgradeNotYetWired",
                "upgrade",
                "hot-upgrade lifecycle invocation is pending explicit ratification",
            ),
        }
    }
    lowered
}
fn push_lifecycle_not_wired_diagnostic(
    diagnostics: &mut Vec<MirDiagnostic>,
    actor_name: &str,
    hook_name: &str,
    diagnostic_name: &str,
    hook_kind: &str,
    reason: &str,
) {
    diagnostics.push(MirDiagnostic {
        kind: MirDiagnosticKind::UnsupportedNode {
            reason: format!("{diagnostic_name}: #[on({hook_kind})] is not wired"),
        },
        note: format!(
            "`#[on({hook_kind})]` hook `{actor_name}::{hook_name}` would silently never run; {reason}"
        ),
    });
}
/// The `$`-mangled symbol base for an actor's native symbols.
///
/// `bank.Account` → `bank$Account` via the shared `mangle_dotted_name`
/// authority (the imported-fn `greeting$hello` scheme); root actors
/// (`defining_module == None`) map to their bare name, so single-module
/// programs keep byte-identical symbols. EVERY `mangle_actor_*` call site
/// feeds this base — never `actor.name` directly — so the MIR producer and
/// the codegen `get_function` consumer cannot drift.
pub(super) fn actor_symbol_base(actor: &HirActorDecl) -> String {
    hew_hir::mangle_dotted_name(&actor.qualified_name())
}
/// Deterministic actor receive-handler symbol mangling.
///
/// Scheme: `<Actor>__recv__<handler>`, with source identifiers preserved
/// verbatim. The module-level actor lowering loop rejects collisions with
/// existing function symbols and earlier handler symbols before body emission.
fn mangle_actor_receive_handler(actor_name: &str, handler_name: &str) -> String {
    format!("{actor_name}__recv__{handler_name}")
}
pub(super) fn mangle_actor_init_handler(actor_name: &str) -> String {
    format!("{actor_name}__init")
}
pub(super) fn mangle_actor_start_handler(actor_name: &str) -> String {
    format!("{actor_name}__on_start")
}
/// Per-hook stop-handler symbol: `<Actor>__on_stop__<hook_idx>`.
///
/// `hook_idx` is the position of this hook in the actor's full
/// `lifecycle_hooks` vec, matching the index used when populating
/// `ActorLayout.on_stop_symbols`. Multiple `#[on(stop)]` hooks on the
/// same actor each get a unique symbol; codegen synthesises a fan-out
/// trampoline that calls them all in this lexical order.
pub(super) fn mangle_actor_stop_handler_indexed(actor_name: &str, hook_idx: usize) -> String {
    format!("{actor_name}__on_stop__{hook_idx}")
}
pub(super) fn mangle_actor_crash_handler(actor_name: &str) -> String {
    format!("{actor_name}__on_crash")
}
pub(super) fn mangle_actor_exit_handler(actor_name: &str) -> String {
    format!("{actor_name}__on_exit")
}
pub(super) fn mangle_actor_down_handler(actor_name: &str) -> String {
    format!("{actor_name}__on_down")
}
/// Per-actor lifecycle-wrapper symbol: `__hew_lifecycle_<mangled-actor>`.
///
/// `actor_name` is the registry key (the dotted qualified name for module
/// actors); the wrapper symbol mangles it through `mangle_dotted_name` so it
/// is collision-free across modules, matching the authority codegen uses when
/// it emits the wrapper body. Stored on `SupervisorChildLayout.lifecycle_symbol`
/// only when the actor declares an `init` or a `#[on(start)]` hook; codegen
/// re-derives the same symbol from `child.actor_name` at the wrapper emit site.
pub(super) fn mangle_actor_lifecycle_wrapper(actor_name: &str) -> String {
    format!(
        "__hew_lifecycle_{}",
        hew_hir::mangle_dotted_name(actor_name)
    )
}
/// Deterministic supervisor-bootstrap symbol mangling.
///
/// Scheme: `<Supervisor>__bootstrap`. The bootstrap function carries
/// `FunctionCallConv::Default` — the synthetic MIR body is a stub that
/// codegen overrides wholesale with the `hew_supervisor_*` call sequence
/// in S-D.3, so the bootstrap function itself does not need to carry an
/// execution-context parameter. Supervisors are still actor-likes at the
/// runtime level (the supervisor's own actor is spawned inside
/// `hew_supervisor_start`); the `call_conv` applies only to the bootstrap
/// function's LLVM signature.
pub(super) fn mangle_supervisor_bootstrap(name: &str) -> String {
    format!("{name}__bootstrap")
}
/// Mangle the synthesised step function symbol for a machine declaration.
///
/// The `<Name>__step` symbol is the compiler-internal helper that the
/// public `m.step(event)` call-site rewrite dispatches through. The user
/// never names this symbol; it lives in the `__`-prefixed compiler-reserved
/// namespace.
pub(super) fn mangle_machine_step(name: &str) -> String {
    format!("{name}__step")
}
/// Compute the stable machine-type id tagging every `EmitEvent` a machine's
/// `emit` pushes, and the filter `m.take_emits(ev)` reads back.
///
/// Algorithm: `SipHasher13::new_with_keys(0, 0)` over the UTF-8 bytes of the
/// machine's unqualified type name — the exact stable-hash precedent the
/// actor `msg_type` uses (`hew-types/src/actor_protocol.rs`,
/// `compute_default_msg_id`), except the full 64-bit digest is kept (no
/// truncation to 32 bits) because the runtime ABI's `machine_id` is `u64`.
///
/// This is the ONE hashing authority: MIR computes it once per machine
/// declaration and threads the resulting `u64` through
/// `Instr::MachineEmitPlaceholder` / `Instr::MachineEmitTake`; codegen only
/// transports the value, it never re-derives it.
pub(super) fn machine_emit_type_id(name: &str) -> u64 {
    use siphasher::sip::SipHasher13;
    use std::hash::Hasher;

    let mut hasher = SipHasher13::new_with_keys(0, 0);
    hasher.write(name.as_bytes());
    hasher.finish()
}
/// Synthesise the `<Name>__step(self, event) -> Name` MIR function for a
/// machine declaration.
///
/// # Signature
///
/// - `self: <Name>` — the machine value, passed by value. Mutation is
///   modelled as returning a new value; the public `m.step(ev)` rewrite
///   is responsible for store-back into the caller's binding.
/// - `event: <Name>Event` — the companion event enum, registered by
///   `register_machine_decl` as a `TypeDefKind::Enum`.
/// - return `<Name>` — the next machine value.
///
/// For generic machines (`type_params` non-empty), the parameters
/// reference type parameters by name via `ResolvedTy::Named` with empty
/// `args`, matching the convention `hew-types` uses for unbound type
/// variables in registered type definitions. The synthesised function is
/// emitted once per machine declaration (not per monomorphisation);
/// monomorphisation-aware codegen for generic machines arrives with the
/// stdlib machine catalogue.
/// # Dispatch shell and transition bodies (Slice 4b)
///
/// The entry block loads the state tag (`Place::MachineTag(self)`)
/// and event tag (`Instr::EnumTagLoad` from the event parameter), then
/// cascades a chain of state-equality checks. Each matched state-block
/// cascades event-equality checks for that state's declared transitions
/// (in source order). Wildcard transitions (`_ -> Target`) are appended
/// to every state's cascade after the specific arms. When an arm carries
/// a `when <guard>` clause, an event match is not sufficient: a guard
/// block evaluates `transition.guard` (in the same self/event binding env
/// the transition body sees, before any hook or transition-out drop) and
/// only branches into the arm body if the guard is true; a false guard
/// falls through to the next arm exactly as an event mismatch would.
/// Undeclared `(state, event)` pairs — and, once any transition in the
/// machine carries a guard, an all-guards-false cell too — fall through
/// to `Terminator::Trap`. Guard-free machines trap with
/// `TrapKind::MachineDispatchUnreachable`, which codegen lowers to a bare
/// LLVM `unreachable`: HIR exhaustiveness checks already guarantee that
/// trap is dead code in well-typed programs. Guard-bearing machines
/// instead trap with `TrapKind::ExhaustivenessFallthrough`, a real,
/// user-reachable trap code — reaching the fall-through is no longer a
/// compiler-invariant violation once a guard can evaluate false at
/// runtime, so lowering it to `unreachable` would be UB. Both are the
/// fail-closed runtime backstop (LESSONS P0 `fail-closed-not-pretend` /
/// `match-fail-closed`).
///
/// Matched arms lower the transition body in-place and return the machine
/// value produced by that HIR body. Real transitions (state tag changes)
/// invoke source `exit` before the body and target `entry` after the body has
/// constructed the target value. Self-transitions skip hooks and transition-out
/// drops unless the HIR transition carries `@reenter`, in which case exit,
/// source-payload drops, and entry all fire.
#[allow(
    clippy::too_many_arguments,
    clippy::too_many_lines,
    reason = "module context plumbing mirrors lower_function; threading individually keeps \
              parity with the rest of the lowering surface. The dispatch tree synthesis \
              keeps the entry / state-cascade / event-cascade / arm-body / hook / trap \
              control flow in one place for review"
)]
pub(super) fn synthesize_machine_step_fn(
    md: &HirMachineDecl,
    type_classes: &hew_hir::TypeClassTable,
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    actor_layouts: &HashMap<String, ActorLayout>,
    supervisor_layout_map: &HashMap<String, crate::model::SupervisorLayout>,
    machine_layout_names: &HashSet<String>,
    enum_layouts: &[crate::model::EnumLayout],
    module_fn_names: &HashSet<String>,
    module_generic_fn_names: &HashSet<String>,
    funcupdate_fn_returns_fresh: &Rc<HashMap<hew_hir::ItemId, bool>>,
    call_scrutinee_provenance: &Rc<crate::return_provenance::CallScrutineeProvenance>,
    param_ownership: &Rc<ParamOwnershipFacts>,
    call_site_type_args: &HashMap<hew_hir::SiteId, Vec<ResolvedTy>>,
    supervisor_child_slots: &HashMap<hew_hir::SiteId, ChildSlot>,
    actor_send_aliasing: &HashMap<hew_types::SpanKey, hew_types::ActorSendAliasing>,
    pointer_width: PointerWidth,
) -> LoweredFunction {
    let emit_name = mangle_machine_step(&md.name);

    // For generic machines, build the self-type with each type-param as a
    // free `ResolvedTy::Named` arg (the same convention as registration in
    // `hew-types`). Non-generic machines have an empty args vec.
    let type_args: Vec<ResolvedTy> = md.type_params.iter().map(|_| ResolvedTy::I64).collect();
    let self_ty = ResolvedTy::Named {
        name: md.name.clone(),
        args: type_args.clone(),
        builtin: None,
        is_opaque: false,
    };
    let event_ty = ResolvedTy::Named {
        name: format!("{}Event", md.name),
        args: type_args.clone(),
        builtin: None,
        is_opaque: false,
    };
    let return_ty = self_ty.clone();

    // Synthetic BindingIds for the step fn's two parameters. We use the
    // top of the u32 range so a real user binding (allocated monotonically
    // from 0 by the module IdGen) can never collide with these IDs even in
    // pathologically large modules. Slice 4c (drop-elaborator) will key
    // tag-dominance facts on these IDs and a collision would silently
    // alias a real binding into the machine-tag slot — making the IDs
    // distinguishable by construction.
    let self_binding = BindingId(u32::MAX);
    let event_binding = BindingId(u32::MAX - 1);

    // Construct a Builder with full module context for parity with user
    // functions. Slice 4a does not lower transition bodies; the context is
    // retained so the function shape plugs into the same MIR containers as
    // every other lowered function.
    let mut builder = Builder {
        type_classes: type_classes.clone(),
        record_field_orders: record_field_orders.clone(),
        actor_layouts: actor_layouts.clone(),
        supervisor_layout_map: supervisor_layout_map.clone(),
        machine_layout_names: machine_layout_names.clone(),
        enum_layouts: enum_layouts.to_vec(),
        module_fn_names: module_fn_names.clone(),
        module_generic_fn_names: module_generic_fn_names.clone(),
        funcupdate_fn_returns_fresh: funcupdate_fn_returns_fresh.clone(),
        call_scrutinee_provenance: call_scrutinee_provenance.clone(),
        param_ownership: param_ownership.clone(),
        call_site_type_args: call_site_type_args.clone(),
        supervisor_child_slots: supervisor_child_slots.clone(),
        actor_send_aliasing: actor_send_aliasing.clone(),
        pointer_width,
        current_function_symbol: emit_name.clone(),
        ..Builder::default()
    };
    for param in &md.type_params {
        builder.subst.insert(param.clone(), ResolvedTy::I64);
    }

    // Allocate parameter locals. `self` → Local(0), `event` → Local(1).
    let self_place = builder.alloc_local(self_ty.clone());
    let event_place = builder.alloc_local(event_ty.clone());
    builder.binding_locals.insert(self_binding, self_place);
    builder.binding_locals.insert(event_binding, event_place);

    // Resolve the MIR-local id of the `self` parameter so `Place::MachineTag`
    // can address it directly (codegen has no `BindingId → local` map; the
    // place primitive carries the local id, see `Place::MachineTag` doc).
    let Place::Local(self_local) = self_place else {
        unreachable!("alloc_local returns Place::Local");
    };

    // Entry block: load state tag and event tag into integer scratch locals,
    // then enter the state-cascade.
    let state_tag = builder.alloc_local(ResolvedTy::I64);
    builder.instructions.push(Instr::Move {
        dest: state_tag,
        src: Place::MachineTag(self_local),
    });
    let event_tag = builder.alloc_local(ResolvedTy::I64);
    builder.instructions.push(Instr::EnumTagLoad {
        src: event_place,
        dest: event_tag,
    });

    // Collect wildcard transitions (source_state == "_") once — they apply
    // to every concrete state's event cascade after the specific arms.
    let wildcard_transitions: Vec<&HirMachineTransition> = md
        .transitions
        .iter()
        .filter(|t| t.source_state == "_")
        .collect();

    // Pre-allocate the trap block id (default fall-through) and the
    // per-state-check block ids. The CFG shape is:
    //   entry -> state_check_0 -> state_check_1 -> ... -> state_check_N -> trap
    //                |               |                       |
    //              state_body_0   state_body_1            state_body_N
    //              (event cascade)
    //
    // Each state_body_i terminates at the dispatching arm's Return, or
    // falls through to `trap` if no event matches.
    let trap_block = builder.alloc_block();

    // Allocate state-check and state-body blocks in pairs.
    let state_check_blocks: Vec<u32> = (0..md.states.len())
        .map(|_| builder.alloc_block())
        .collect();
    let state_body_blocks: Vec<u32> = (0..md.states.len())
        .map(|_| builder.alloc_block())
        .collect();

    // Entry block flows into the first state-check (or the trap if there
    // are no states, which HIR rejects but we handle defensively).
    let first_check = state_check_blocks.first().copied().unwrap_or(trap_block);
    builder.finish_current_block(Terminator::Goto {
        target: first_check,
    });

    // Build each state's check and body block.
    for (state_idx, state) in md.states.iter().enumerate() {
        // ── state_check_i: compare state_tag against this state's index.
        builder.start_block(state_check_blocks[state_idx]);
        let state_const = builder.alloc_local(ResolvedTy::I64);
        builder.instructions.push(Instr::ConstI64 {
            dest: state_const,
            value: i64::try_from(state_idx).unwrap_or(i64::MAX),
        });
        let state_eq = builder.alloc_local(ResolvedTy::Bool);
        builder.instructions.push(Instr::IntCmp {
            dest: state_eq,
            pred: CmpPred::Eq,
            lhs: state_tag,
            rhs: state_const,
        });
        let next_check = state_check_blocks
            .get(state_idx + 1)
            .copied()
            .unwrap_or(trap_block);
        builder.finish_current_block(Terminator::Branch {
            cond: state_eq,
            then_target: state_body_blocks[state_idx],
            else_target: next_check,
        });

        // ── state_body_i: event cascade. Concrete (state==this) transitions
        // first in source order, then wildcard transitions in source order.
        // Both groups are filtered by event so each event_idx is matched at
        // most once per state.
        builder.start_block(state_body_blocks[state_idx]);
        let mut arms: Vec<(usize, &HirMachineTransition, bool)> = Vec::new();
        for t in &md.transitions {
            if t.source_state == state.name {
                if let Some(ev_idx) = md.events.iter().position(|ev| ev.name == t.event_name) {
                    arms.push((ev_idx, t, false));
                }
            }
        }
        for t in &wildcard_transitions {
            if let Some(ev_idx) = md.events.iter().position(|ev| ev.name == t.event_name) {
                // Skip the wildcard only when an UNGUARDED specific arm for
                // the same event already exists in this state — HIR's
                // duplicate-transition guard forbids two unconditional arms
                // for one (state, event) cell, so an unguarded specific arm
                // is unconditionally reachable and the wildcard could never
                // fire anyway. A *guarded* specific arm is conditional: on a
                // false guard at runtime, dispatch must still fall through to
                // the wildcard rather than the trap, so a same-event wildcard
                // is appended as this event's final arm whenever every
                // specific arm seen so far for it carries a guard (see #2390:
                // filtering on "any arm" here made the wildcard permanently
                // unreachable behind so much as one guarded sibling, and a
                // false guard fell all the way to ExhaustivenessFallthrough).
                let unguarded_specific_arm_exists = arms.iter().any(|(idx, arm_t, is_wildcard)| {
                    *idx == ev_idx && !*is_wildcard && arm_t.guard.is_none()
                });
                if !unguarded_specific_arm_exists {
                    arms.push((ev_idx, t, true));
                }
            }
        }

        // Emit the event cascade. Each arm: ConstI64(event_idx), IntCmp,
        // Branch(then=arm_body, else=next_arm_check) — or, when the arm
        // carries a `when` guard, Branch(then=guard_block, else=next_arm_check)
        // with the guard_block itself branching (then=arm_body,
        // else=next_arm_check). Last arm's else points at the trap.
        let arm_check_blocks: Vec<u32> = (0..arms.len()).map(|_| builder.alloc_block()).collect();
        let arm_body_blocks: Vec<u32> = (0..arms.len()).map(|_| builder.alloc_block()).collect();
        // state_body cascades directly to first arm_check (or trap if no arms).
        let first_arm_check = arm_check_blocks.first().copied().unwrap_or(trap_block);
        builder.finish_current_block(Terminator::Goto {
            target: first_arm_check,
        });

        for (arm_idx, (ev_idx, transition, _is_wildcard)) in arms.iter().enumerate() {
            // ── arm_check_j: compare event_tag against this arm's event_idx.
            builder.start_block(arm_check_blocks[arm_idx]);
            let ev_const = builder.alloc_local(ResolvedTy::I64);
            builder.instructions.push(Instr::ConstI64 {
                dest: ev_const,
                value: i64::try_from(*ev_idx).unwrap_or(i64::MAX),
            });
            let ev_eq = builder.alloc_local(ResolvedTy::Bool);
            builder.instructions.push(Instr::IntCmp {
                dest: ev_eq,
                pred: CmpPred::Eq,
                lhs: event_tag,
                rhs: ev_const,
            });
            let next_arm = arm_check_blocks
                .get(arm_idx + 1)
                .copied()
                .unwrap_or(trap_block);

            // A guarded arm inserts a guard-check block between the
            // event-eq branch and the arm body: the event matching this
            // arm is necessary but not sufficient — the guard must also
            // evaluate true, or dispatch falls through to the next arm
            // (or the trap) exactly as an event mismatch would. Unguarded
            // arms branch straight to their body, unchanged.
            let event_matched_target = if transition.guard.is_some() {
                builder.alloc_block()
            } else {
                arm_body_blocks[arm_idx]
            };
            builder.finish_current_block(Terminator::Branch {
                cond: ev_eq,
                then_target: event_matched_target,
                else_target: next_arm,
            });

            if let Some(guard_expr) = &transition.guard {
                // ── guard_block: evaluate `transition.guard` in the same
                // self/event binding env the transition body sees (the
                // exact dance `emit_machine_step_transition_return` uses),
                // BEFORE any hook or transition-out drop runs — guards are
                // pure reads and must not observe or trigger side effects.
                builder.start_block(event_matched_target);
                let prev_machine_self = builder.current_machine_self_binding.replace(self_binding);
                let prev_machine_event =
                    builder.current_machine_event_binding.replace(event_binding);
                let prev_self_place = builder.binding_locals.insert(self_binding, self_place);
                let prev_event_place = builder.binding_locals.insert(event_binding, event_place);
                let guard_val = builder.lower_value(guard_expr);
                if let Some(place) = prev_self_place {
                    builder.binding_locals.insert(self_binding, place);
                } else {
                    builder.binding_locals.remove(&self_binding);
                }
                if let Some(place) = prev_event_place {
                    builder.binding_locals.insert(event_binding, place);
                } else {
                    builder.binding_locals.remove(&event_binding);
                }
                builder.current_machine_self_binding = prev_machine_self;
                builder.current_machine_event_binding = prev_machine_event;

                if let Some(cond) = guard_val {
                    builder.finish_current_block(Terminator::Branch {
                        cond,
                        then_target: arm_body_blocks[arm_idx],
                        else_target: next_arm,
                    });
                } else {
                    // The checker guarantees every guard is a `Ty::Bool`
                    // expression; a lowering failure here is a compiler
                    // invariant violation, not a user-reachable error.
                    // Fail closed: treat it as if the guard were false
                    // rather than fabricate a true branch into the arm.
                    builder.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::UnsupportedNode {
                            reason: format!(
                                "machine `{}` transition `on {}` from `{}` to `{}` guard \
                                 did not produce a value",
                                md.name,
                                transition.event_name,
                                transition.source_state,
                                transition.target_state
                            ),
                        },
                        note: "guard expressions must lower to a boolean value before the \
                               step arm can branch on it"
                            .to_string(),
                    });
                    builder.finish_current_block(Terminator::Goto { target: next_arm });
                }
            }

            // ── arm_body_j: run hooks/body and return the next state.
            builder.start_block(arm_body_blocks[arm_idx]);
            emit_machine_step_transition_return(
                &mut builder,
                md,
                state,
                state_idx,
                transition,
                (self_binding, self_place),
                (event_binding, event_place),
            );
            builder.finish_current_block(Terminator::Return);
        }
    }

    // Default fall-through. Two cases:
    //
    //   * `default { state }` present (`md.has_default`): an unhandled
    //     `(state, event)` cell STAYS in the current state. Return `self`
    //     unchanged — no lifecycle hooks run because the state did not change.
    //   * no `default`: the dispatch is exhaustive by construction (HIR's
    //     exhaustiveness check rejects any uncovered cell), so reaching this
    //     block is a compiler invariant violation. Fail closed with a trap.
    //
    // Without the `has_default` branch the fall-through unconditionally trapped,
    // so a `default` machine compiled but ran broken: the first unhandled cell
    // garbled `self` instead of staying put (the Q379 bug).
    builder.start_block(trap_block);
    if md.has_default {
        builder.instructions.push(Instr::Move {
            dest: Place::ReturnSlot,
            src: self_place,
        });
        builder.finish_current_block(Terminator::Return);
    } else if md.transitions.iter().any(|t| t.guard.is_some()) {
        // A guarded transition can fail its guard at runtime, so a
        // no-`default` machine with at least one guard can genuinely
        // reach this fall-through on a well-typed program (all guards for
        // the matched state×event cell evaluated false). That is no
        // longer a compiler-invariant violation — it is a user-reachable
        // condition — so it must trap with a real, documented exit code
        // instead of lowering to LLVM `unreachable` (which would be UB).
        builder.finish_current_block(Terminator::Trap {
            kind: TrapKind::ExhaustivenessFallthrough,
        });
    } else {
        builder.finish_current_block(Terminator::Trap {
            kind: TrapKind::MachineDispatchUnreachable,
        });
    }

    // Drain pending blocks into the function's final blocks vec. The
    // builder's current block is the trap (already finished); there is no
    // dangling cursor. We allocate a sentinel empty entry to satisfy
    // `finalize_blocks` (which seals the current cursor with a terminator);
    // start_block to a fresh id and immediately finalize with a benign
    // Goto-to-trap that will be unreachable, OR just collect the pending
    // blocks directly.
    //
    // Inline the drain: take pending_blocks directly, sort by id. Equivalent
    // to finalize_blocks but without adding a phantom block for the cursor.
    let mut blocks = std::mem::take(&mut builder.pending_blocks);
    blocks.sort_by_key(|b| b.id);

    // Locals + diagnostics drained from the builder.
    let locals = builder.locals.clone();
    let params = vec![self_ty.clone(), event_ty.clone()];
    let diagnostics = std::mem::take(&mut builder.diagnostics);

    let raw = RawMirFunction {
        name: emit_name.clone(),
        return_ty: return_ty.clone(),
        call_conv: crate::model::FunctionCallConv::Default,
        params,
        locals,
        // Synthesised machine-`step` dispatch: no faithful source bindings,
        // so no `-g` variable DIEs (consistent with `span: None`).
        local_names: Vec::new(),
        local_scopes: Vec::new(),
        local_decl_bytes: Vec::new(),
        scope_table: Vec::new(),
        blocks: blocks.clone(),
        decisions: Vec::new(),
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),
        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
        source_origin: SourceOrigin::SynthesizedMachineStep {
            machine_name: md.name.clone(),
        },
    };

    let thir = ThirFunction {
        name: emit_name.clone(),
        return_ty: return_ty.clone(),
        statements: Vec::new(),
    };

    let checked = CheckedMirFunction {
        name: emit_name.clone(),
        return_ty: return_ty.clone(),
        blocks: blocks.clone(),
        decisions: Vec::new(),
        checks: Vec::new(),
        cooperate_sites: Vec::new(),
    };

    // Elaborated: mirror raw blocks as Normal kind with no owned-local drops.
    // Machine transition-out resource drops are emitted directly into the raw
    // arm body because their liveness is tag-dominated by the source state.
    let elab_blocks: Vec<ElabBlock> = blocks
        .iter()
        .map(|b| ElabBlock {
            id: b.id,
            kind: BlockKind::Normal,
            drops: Vec::new(),
            successor: None,
        })
        .collect();
    let elaborated = ElaboratedMirFunction {
        name: emit_name,
        return_ty,
        statements: Vec::new(),
        decisions: Vec::new(),
        blocks: elab_blocks,
        drop_plans: Vec::new(),
        coroutine: None,
        lambda_captures: Vec::new(),
    };

    LoweredFunction {
        thir,
        raw,
        checked,
        elaborated,
        diagnostics,
        generated: Vec::new(),
        record_layouts: Vec::new(),
    }
}
fn emit_machine_step_transition_return(
    builder: &mut Builder,
    md: &HirMachineDecl,
    state: &hew_hir::HirMachineState,
    state_idx: usize,
    transition: &HirMachineTransition,
    self_info: (BindingId, Place),
    event_info: (BindingId, Place),
) {
    let (self_binding, self_place) = self_info;
    let (event_binding, event_place) = event_info;
    let machine_emit_id = machine_emit_type_id(&md.name);

    let target_idx = md
        .states
        .iter()
        .position(|s| s.name == transition.target_state)
        .unwrap_or(state_idx);
    let invokes_lifecycle_hooks = target_idx != state_idx || transition.reenter;

    if invokes_lifecycle_hooks {
        if let Some(exit) = &state.exit {
            lower_machine_lifecycle_block(builder, self_binding, self_place, exit, machine_emit_id);
        }
        emit_machine_transition_out_drops(builder, state, state_idx, self_place);
    }

    let prev_machine_self = builder.current_machine_self_binding.replace(self_binding);
    let prev_machine_event = builder.current_machine_event_binding.replace(event_binding);
    let prev_machine_emit_type_id = builder
        .current_machine_emit_type_id
        .replace(machine_emit_id);
    let prev_self_place = builder.binding_locals.insert(self_binding, self_place);
    let prev_event_place = builder.binding_locals.insert(event_binding, event_place);
    let next = if is_machine_state_passthrough(&transition.body) {
        Some(self_place)
    } else {
        builder.lower_value(&transition.body)
    };
    if let Some(place) = prev_self_place {
        builder.binding_locals.insert(self_binding, place);
    } else {
        builder.binding_locals.remove(&self_binding);
    }
    if let Some(place) = prev_event_place {
        builder.binding_locals.insert(event_binding, place);
    } else {
        builder.binding_locals.remove(&event_binding);
    }
    builder.current_machine_self_binding = prev_machine_self;
    builder.current_machine_event_binding = prev_machine_event;
    builder.current_machine_emit_type_id = prev_machine_emit_type_id;

    let Some(next) = next else {
        builder.diagnostics.push(MirDiagnostic {
            kind: MirDiagnosticKind::UnsupportedNode {
                reason: format!(
                    "machine `{}` transition `on {}` from `{}` to `{}` did not produce \
                     a next-state value",
                    md.name,
                    transition.event_name,
                    transition.source_state,
                    transition.target_state
                ),
            },
            note: "transition bodies must lower to a machine state constructor before the \
                   step arm can write the return slot"
                .to_string(),
        });
        return;
    };

    let Place::Local(_) = next else {
        builder.diagnostics.push(MirDiagnostic {
            kind: MirDiagnosticKind::UnsupportedNode {
                reason: format!(
                    "machine `{}` transition `on {}` produced non-local next-state place {next:?}",
                    md.name, transition.event_name
                ),
            },
            note:
                "machine transition bodies must materialise a local machine value so entry hooks \
                   can observe the constructed target"
                    .to_string(),
        });
        return;
    };

    if invokes_lifecycle_hooks {
        if let Some(target_state) = md.states.get(target_idx) {
            if let Some(entry) = &target_state.entry {
                lower_machine_lifecycle_block(builder, self_binding, next, entry, machine_emit_id);
            }
        }
    }

    builder.instructions.push(Instr::Move {
        dest: Place::ReturnSlot,
        src: next,
    });
}
fn is_machine_state_passthrough(expr: &hew_hir::HirExpr) -> bool {
    match &expr.kind {
        HirExprKind::BindingRef {
            name,
            resolved: ResolvedRef::Binding(_),
        } if name == "state" => true,
        HirExprKind::Block(block) => block
            .tail
            .as_deref()
            .is_some_and(is_machine_state_passthrough),
        _ => false,
    }
}
/// Build the per-machine layout descriptor from its HIR declaration.
///
/// `tag_width` is the minimum bit width to index all states:
/// max(1, `ceil(log2(state_count))`). A single-state machine uses 1 bit
/// (the tag field is always present) so `Place::MachineTag` is always
/// addressable. State and event payload field types ride the
/// v0.5 generic-machine doctrine: type parameters default to `i64`
/// (`default_machine_type_params_to_i64`); non-`i64` instantiations fail
/// closed downstream at codegen's Move type check, so the defaulted
/// layout is exact for every instantiation that can actually run.
pub(super) fn build_machine_layout(md: &HirMachineDecl) -> crate::model::MachineLayout {
    let state_count = u32::try_from(md.states.len().max(1)).unwrap_or(u32::MAX);
    let tag_width = u32::max(1, state_count.next_power_of_two().trailing_zeros());
    crate::model::MachineLayout {
        name: md.name.clone(),
        tag_width,
        variants: md
            .states
            .iter()
            .map(|s| crate::model::MachineVariantLayout {
                name: s.name.clone(),
                field_tys: s
                    .fields
                    .iter()
                    .map(|f| default_machine_type_params_to_i64(&f.ty, md))
                    .collect(),
                field_names: s.fields.iter().map(|f| f.name.clone()).collect(),
            })
            .collect(),
        events: md
            .events
            .iter()
            .map(|e| crate::model::MachineVariantLayout {
                name: e.name.clone(),
                field_tys: e
                    .fields
                    .iter()
                    .map(|f| default_machine_type_params_to_i64(&f.ty, md))
                    .collect(),
                field_names: e.fields.iter().map(|f| f.name.clone()).collect(),
            })
            .collect(),
    }
}
fn default_machine_type_params_to_i64(ty: &ResolvedTy, md: &HirMachineDecl) -> ResolvedTy {
    match ty {
        ResolvedTy::Named { name, args, .. }
            if args.is_empty() && md.type_params.contains(name) =>
        {
            ResolvedTy::I64
        }
        ResolvedTy::Named {
            name,
            args,
            builtin,
            is_opaque,
        } => ResolvedTy::Named {
            name: name.clone(),
            args: args
                .iter()
                .map(|arg| default_machine_type_params_to_i64(arg, md))
                .collect(),
            builtin: *builtin,
            is_opaque: *is_opaque,
        },
        ResolvedTy::Tuple(elems) => ResolvedTy::Tuple(
            elems
                .iter()
                .map(|elem| default_machine_type_params_to_i64(elem, md))
                .collect(),
        ),
        ResolvedTy::Function { params, ret } => ResolvedTy::Function {
            params: params
                .iter()
                .map(|param| default_machine_type_params_to_i64(param, md))
                .collect(),
            ret: Box::new(default_machine_type_params_to_i64(ret, md)),
        },
        ResolvedTy::Closure {
            params,
            ret,
            captures,
        } => ResolvedTy::Closure {
            params: params
                .iter()
                .map(|param| default_machine_type_params_to_i64(param, md))
                .collect(),
            ret: Box::new(default_machine_type_params_to_i64(ret, md)),
            captures: captures
                .iter()
                .map(|capture| default_machine_type_params_to_i64(capture, md))
                .collect(),
        },
        ResolvedTy::Pointer {
            is_mutable,
            pointee,
        } => ResolvedTy::Pointer {
            is_mutable: *is_mutable,
            pointee: Box::new(default_machine_type_params_to_i64(pointee, md)),
        },
        ResolvedTy::Task(inner) => {
            ResolvedTy::Task(Box::new(default_machine_type_params_to_i64(inner, md)))
        }
        other => other.clone(),
    }
}
fn emit_machine_transition_out_drops(
    builder: &mut Builder,
    state: &hew_hir::HirMachineState,
    state_idx: usize,
    self_place: Place,
) {
    let Place::Local(self_local) = self_place else {
        builder.diagnostics.push(MirDiagnostic {
            kind: MirDiagnosticKind::UnsupportedNode {
                reason: format!(
                    "machine state `{}` transition-out drop has non-local self place {self_place:?}",
                    state.name
                ),
            },
            note: "internal: machine step self parameter must be a Place::Local for \
                   MachineVariant drop addressing"
                .to_string(),
        });
        return;
    };

    let variant_idx = u32::try_from(state_idx).expect("state index exceeds u32::MAX");
    // Enumerate only the fields of the source variant proven live by the arm's
    // dominating MachineTag check; inactive union payload bytes are not touched.
    for (field_idx, field) in state.fields.iter().enumerate().rev() {
        if ValueClass::of_ty(&field.ty, &builder.type_classes) != ValueClass::AffineResource {
            continue;
        }
        let field_idx = u32::try_from(field_idx).expect("field index exceeds u32::MAX");
        let place = Place::MachineVariant {
            local: self_local,
            variant_idx,
            field_idx,
        };
        builder.instructions.push(Instr::Drop {
            place,
            ty: field.ty.clone(),
            drop_fn: resource_drop_fn(&field.ty, &builder.type_classes),
        });
    }
}
fn lower_machine_lifecycle_block(
    builder: &mut Builder,
    self_binding: BindingId,
    self_place: Place,
    block: &hew_hir::HirBlock,
    machine_emit_id: u64,
) {
    let prev_machine_self = builder.current_machine_self_binding.replace(self_binding);
    let prev_machine_emit_type_id = builder
        .current_machine_emit_type_id
        .replace(machine_emit_id);
    let prev_self_place = builder.binding_locals.insert(self_binding, self_place);
    for stmt in &block.statements {
        builder.stmt(stmt);
    }
    if let Some(tail) = &block.tail {
        let _ = builder.lower_value(tail);
    }
    if let Some(place) = prev_self_place {
        builder.binding_locals.insert(self_binding, place);
    } else {
        builder.binding_locals.remove(&self_binding);
    }
    builder.current_machine_self_binding = prev_machine_self;
    builder.current_machine_emit_type_id = prev_machine_emit_type_id;
}
/// Topologically sort supervisor children by `wired_to:` dependencies via
/// Kahn's algorithm.
///
/// Returns child references in spawn order — dependencies first, dependents
/// after. Siblings that share no dependency relationship preserve source
/// declaration order, courtesy of the FIFO queue.
///
/// Returns `None` if a cycle is detected. This is the MIR-side fail-closed
/// backstop; the primary cycle gate is S-B's
/// `check_supervisor_wired_to_cycles` in `hew-types`, which emits the
/// user-facing `E_SUPERVISOR_WIRED_CYCLE` diagnostic. A cycle reaching
/// MIR means either the checker was bypassed or has a bug; MIR refuses
/// to emit a bootstrap function in that case rather than infinite-loop
/// on the dep graph.
/// True when a child init-arg HIR expr is a `config.field` read — a
/// `FieldAccess` whose object is a binding reference (the supervisor config
/// param). This is the same shape the MIR `ConfigField` arm matches, used here
/// to skip the synthetic bootstrap body's spawn for config-init children (the
/// codegen init thunk produces their state; the synthetic body is discarded).
fn hir_expr_reads_config_field(expr: &HirExpr) -> bool {
    matches!(
        &expr.kind,
        HirExprKind::FieldAccess { object, .. }
            if matches!(object.kind, HirExprKind::BindingRef { .. })
    )
}
fn supervisor_children_in_spawn_order(sup: &HirSupervisorDecl) -> Option<Vec<&HirSupervisorChild>> {
    use std::collections::VecDeque;

    let child_names: HashSet<&str> = sup.children.iter().map(|c| c.name.as_str()).collect();

    // Build dep list: child_name -> list of sibling names it depends on.
    // Filter out unknown sibling names (S-B reports those; we treat them
    // as no-dep edges so the topo sort still produces a deterministic
    // order over the children that DO exist).
    let mut in_degree: HashMap<&str, usize> = sup
        .children
        .iter()
        .map(|c| (c.name.as_str(), 0usize))
        .collect();
    let mut dependents_of: HashMap<&str, Vec<&str>> = sup
        .children
        .iter()
        .map(|c| (c.name.as_str(), vec![]))
        .collect();

    for child in &sup.children {
        let Some(wired) = &child.wired_to else {
            continue;
        };
        for sibling in wired.values() {
            let sibling_str = sibling.as_str();
            if !child_names.contains(sibling_str) {
                continue;
            }
            // `child` depends on `sibling` -> edge sibling -> child.
            if let Some(deps) = dependents_of.get_mut(sibling_str) {
                deps.push(child.name.as_str());
            }
            if let Some(d) = in_degree.get_mut(child.name.as_str()) {
                *d += 1;
            }
        }
    }

    // FIFO queue preserves declaration order for in-degree-zero siblings.
    let mut queue: VecDeque<&str> = sup
        .children
        .iter()
        .filter(|c| in_degree.get(c.name.as_str()).copied().unwrap_or(0) == 0)
        .map(|c| c.name.as_str())
        .collect();

    // Look up child by name for the result vector.
    let by_name: HashMap<&str, &HirSupervisorChild> =
        sup.children.iter().map(|c| (c.name.as_str(), c)).collect();

    let mut ordered: Vec<&HirSupervisorChild> = Vec::with_capacity(sup.children.len());
    while let Some(name) = queue.pop_front() {
        if let Some(child) = by_name.get(name).copied() {
            ordered.push(child);
        }
        let dependents = dependents_of.get(name).cloned().unwrap_or_default();
        for dep in dependents {
            if let Some(d) = in_degree.get_mut(dep) {
                *d -= 1;
                if *d == 0 {
                    queue.push_back(dep);
                }
            }
        }
    }

    if ordered.len() != sup.children.len() {
        // Cycle. S-B should have caught this; MIR refuses to emit.
        return None;
    }
    Some(ordered)
}
/// Build a `SupervisorLayout` from an `HirSupervisorDecl`. Returns `None`
/// if the child dependency graph is cyclic — the MIR fail-closed backstop
/// to S-B's `E_SUPERVISOR_WIRED_CYCLE` diagnostic.
pub(super) fn build_supervisor_layout(
    sup: &HirSupervisorDecl,
    diagnostics: &mut Vec<MirDiagnostic>,
) -> Option<crate::model::SupervisorLayout> {
    let Some(ordered) = supervisor_children_in_spawn_order(sup) else {
        diagnostics.push(MirDiagnostic {
            kind: MirDiagnosticKind::UnsupportedNode {
                reason: format!(
                    "supervisor `{}` has a cyclic `wired_to` dependency graph",
                    sup.name
                ),
            },
            note: "the type checker's E_SUPERVISOR_WIRED_CYCLE diagnostic is the \
                   primary gate; MIR is refusing to emit a bootstrap function as \
                   the structural backstop"
                .to_string(),
        });
        return None;
    };

    let children: Vec<crate::model::SupervisorChildLayout> = ordered
        .iter()
        .enumerate()
        .map(|(spawn_order, child)| crate::model::SupervisorChildLayout {
            name: child.name.clone(),
            actor_name: child.ty.clone(),
            restart_policy: child.restart_policy,
            is_pool: child.is_pool,
            slot_index: child.slot_index,
            wired_to: child.wired_to.clone().unwrap_or_default(),
            spawn_order: u32::try_from(spawn_order)
                .expect("supervisor child count exceeds u32::MAX — impossible in Hew"),
            // Populated after actor_layout_map is built (post-loop pass in
            // lower_hir_module) to handle any declaration order. Left None
            // here so build_supervisor_layout needs no actor-layout parameter.
            on_crash_symbol: None,
            lifecycle_symbol: None,
            max_heap_bytes: None,
            cycle_capable: false,
            // Populated in the post-loop pass along with max_heap_bytes /
            // cycle_capable, mirroring the actor's declared mailbox clause.
            mailbox_capacity: None,
            overflow_policy: None,
            // Populated in the post-loop pass along with actor layout fields.
            init_state_fields: Vec::new(),
            // Populated in the post-loop pass, where the full set of declared
            // supervisor names is known. `Some` when this child's type is itself
            // a supervisor (nested supervision tree); `None` for actor children.
            nested_bootstrap_symbol: None,
            // Populated in the post-loop pass from HirSupervisorChild.pool_count
            // (the reserved `count:` arg). `Some` for a pool child; `None` for a
            // static child.
            pool_count: None,
            // accepted-only: the per-child `shutdown:` directive
            // (HirSupervisorChild.shutdown) is parsed, checked, and round-trips
            // through the formatter, but is NOT lowered into the runtime ABI in
            // v0.5 — there is no per-child graceful-stop deadline wheel yet.
            // Threading it here would require a HewChildSpec ABI field the
            // runtime would ignore, which would fake enforcement. When the
            // deadline wheel lands, read child.shutdown here and emit it.
            site: child.site,
        })
        .collect();

    // Lift the construction-time config param (`supervisor App(config: T)`) so
    // codegen can size the config buffer and give the bootstrap a config
    // parameter. v0.6 admits at most one config param; if a future surface
    // allows several, the first names the config struct codegen materialises.
    let config_param = sup.params.first().and_then(|param| {
        if let ResolvedTy::Named { name, .. } = &param.ty {
            Some(crate::model::SupervisorConfigParam {
                name: param.name.clone(),
                config_ty_name: name.clone(),
            })
        } else {
            // A non-named config param type (e.g. a bare scalar) has no record
            // layout to GEP; the checker admits only struct config params for
            // `config.field` reads, so a non-Named param here means no
            // config-field child can resolve. Leave None — codegen then has no
            // config buffer to thread, and any `config.field` child already
            // fail-closed at MIR for the missing Named discriminator.
            None
        }
    });

    Some(crate::model::SupervisorLayout {
        name: sup.name.clone(),
        strategy: sup.strategy,
        max_restarts: sup.max_restarts,
        window: sup.window.clone(),
        bootstrap_symbol: mangle_supervisor_bootstrap(&sup.name),
        children,
        config_param,
    })
}
/// Build the `ResolvedTy` for a `LocalPid<ChildActorName>` handle — the
/// type the checker assigns to `spawn ChildActor(...)` results
/// (`hew-types/src/check/expressions.rs::check_spawn`). The supervisor
/// bootstrap's synthetic HIR uses this type for the let-binding that
/// captures each spawned child's handle and for the `BindingRef`s that
/// pass sibling handles to dependents' `wired_to:` init params.
fn local_pid_of(actor_name: &str) -> ResolvedTy {
    ResolvedTy::Named {
        name: "LocalPid".to_string(),
        args: vec![ResolvedTy::Named {
            name: actor_name.to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        }],
        builtin: Some(hew_types::BuiltinType::LocalPid),
        is_opaque: false,
    }
}
/// Lower an `HirItem::Supervisor` to a `FunctionCallConv::ActorHandler`
/// bootstrap function. The body is a topologically ordered sequence of
/// `spawn <ChildActor>(...)` statements; siblings referenced by
/// `wired_to:` resolve to the earlier-spawned binding's handle and ride
/// the existing `lower_spawn_actor` substrate as `init_args`.
///
/// Substrate reuse: this synthesises a `HirFn` and dispatches through the
/// same `lower_function` pipeline that emits every other function. The
/// fail-closed dataflow checker, the per-block topology builder, and
/// `bracket_actor_handler_blocks` all fire from the `call_conv` discriminator
/// — no per-supervisor variant of any of that machinery exists.
///
/// Q87 note: supervisors have no `receive fn` and no
/// `ActorProtocolDescriptor`. The bootstrap function itself is plain
/// MIR; the `SupervisorLayout` documents the deliberate descriptor
/// absence at its type docstring. This is the load-bearing call: future
/// supervisor-facing message protocols (programmatic restart, drain
/// APIs) would require introducing a descriptor at THAT point — not
/// retrofitting one here for hypothetical use.
///
/// Synthetic HIR IDs (`BindingId`, `SiteId`, `HirNodeId`, `ScopeId`) are
/// per-function counters scoped to the helper. The downstream MIR
/// builder consumes them as opaque tags — `Builder.binding_locals` and
/// the dataflow analyzer key on per-function uniqueness, which the
/// monotonic counter guarantees.
#[allow(
    clippy::too_many_arguments,
    reason = "mirrors lower_actor_init_handler's threading of module-wide tables"
)]
#[allow(
    clippy::too_many_lines,
    reason = "single coherent helper: synthetic-HIR construction + per-child wiring + \
              dispatch through lower_function. Splitting would obscure that the entire \
              function exists to materialize a single bootstrap HirFn whose statements \
              must be built in topological order with stable per-function ids."
)]
pub(super) fn lower_supervisor_bootstrap(
    sup: &HirSupervisorDecl,
    supervisor_layouts: &HashMap<String, crate::model::SupervisorLayout>,
    type_classes: &hew_hir::TypeClassTable,
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    actor_layouts: &HashMap<String, ActorLayout>,
    machine_layout_names: &HashSet<String>,
    enum_layouts: &[crate::model::EnumLayout],
    opaque_handle_names: &[String],
    module_fn_names: &HashSet<String>,
    module_generic_fn_names: &HashSet<String>,
    funcupdate_fn_returns_fresh: &Rc<HashMap<hew_hir::ItemId, bool>>,
    call_scrutinee_provenance: &Rc<crate::return_provenance::CallScrutineeProvenance>,
    param_ownership: &Rc<ParamOwnershipFacts>,
    call_site_type_args: &HashMap<hew_hir::SiteId, Vec<ResolvedTy>>,
    supervisor_child_slots: &HashMap<hew_hir::SiteId, ChildSlot>,
    pool_accessor_sites: &HashMap<hew_hir::SiteId, hew_types::PoolAccessor>,
    actor_send_aliasing: &HashMap<hew_types::SpanKey, hew_types::ActorSendAliasing>,
    pointer_width: PointerWidth,
    emitted_symbols: &mut HashMap<String, String>,
    task_entry_adapter_symbols: &TaskEntryAdapterSymbols,
    diagnostics: &mut Vec<MirDiagnostic>,
) -> Option<LoweredFunction> {
    let emit_name = mangle_supervisor_bootstrap(&sup.name);
    let duplicate_label = format!("supervisor `{}` bootstrap", sup.name);
    if let Some(existing) = emitted_symbols.get(&emit_name) {
        diagnostics.push(MirDiagnostic {
            kind: MirDiagnosticKind::ActorHandlerSymbolCollision {
                symbol: emit_name,
                existing: existing.clone(),
                duplicate: duplicate_label,
            },
            note: "supervisor bootstrap symbol mangling must be one-to-one before MIR emission"
                .to_string(),
        });
        return None;
    }
    emitted_symbols.insert(emit_name.clone(), duplicate_label);

    // Topo-ordered child list. None means cycle (S-B should have caught
    // first; build_supervisor_layout already pushed the structural
    // diagnostic if so).
    let ordered = supervisor_children_in_spawn_order(sup)?;

    // Verify every child names an actor we know about — UNLESS the child's
    // declared type is itself a supervisor (a nested supervision subtree). A
    // nested-supervisor child is brought up by codegen's bootstrap emitter
    // (which calls the child supervisor's own bootstrap and registers it via
    // `hew_supervisor_add_child_supervisor_with_init`), not by this synthetic
    // `spawn ChildActor` stub, so it has no actor layout and must be skipped
    // here. For a genuine actor child, an unknown actor is still a fail-closed
    // boundary (the checker is the user-facing gate; a stale registry desync
    // lands here).
    for child in &ordered {
        if supervisor_layouts.contains_key(&child.ty) {
            continue;
        }
        if !actor_layouts.contains_key(&child.ty) {
            diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!(
                        "supervisor `{}` child `{}` references unknown actor `{}`",
                        sup.name, child.name, child.ty
                    ),
                    // No real SiteId — use a synthetic 0; the user-facing
                    // gate is the checker, which already names the spans.
                    site: SiteId(0),
                },
                note: "MIR could not resolve the child's actor type in the actor-layout \
                       table; the type checker should have rejected this supervisor"
                    .to_string(),
            });
            return None;
        }
    }

    // ── Synthesize the bootstrap HirFn. ──────────────────────────────
    //
    // Body shape (in topo order, one statement per child):
    //
    //     let <child_name>: LocalPid<ChildActor> = spawn ChildActor(
    //         <init_param_1>: <expr_1>,
    //         ...
    //     );
    //
    // For a `wired_to:` mapping `{ init_param: sibling_name }`, the
    // value expression is `BindingRef { name: sibling_name, resolved:
    // Binding(<sibling's id>) }` — the dataflow analyzer sees a Bind +
    // Use chain that rubber-stamps because no Consume intent appears.
    //
    // Non-`wired_to:` init params are NOT lowered here. The current
    // surface only accepts `wired_to:`-driven init params; any other
    // shape is checker-rejected. If/when supervisors gain literal init
    // args, this is the site that grows them.

    let mut next_binding: u32 = 0;
    let mut next_site: u32 = 0;
    let mut next_node: u32 = 0;
    let mut fresh_binding = || {
        let id = BindingId(next_binding);
        next_binding += 1;
        id
    };
    let mut fresh_site = || {
        let id = SiteId(next_site);
        next_site += 1;
        id
    };
    let mut fresh_node = || {
        let id = HirNodeId(next_node);
        next_node += 1;
        id
    };

    // Map: child name -> (binding id of the let, handle type) so wired_to
    // arg expressions can resolve to the earlier spawn's binding.
    let mut child_bindings: HashMap<String, (BindingId, ResolvedTy)> = HashMap::new();

    let mut statements: Vec<HirStmt> = Vec::with_capacity(ordered.len());
    for child in &ordered {
        // Nested-supervisor children are registered by codegen's bootstrap
        // emitter (it calls the child's own bootstrap and attaches it via
        // `hew_supervisor_add_child_supervisor_with_init`). They have no
        // `spawn ChildActor` statement in this synthetic stub — skip them so
        // the stub does not synthesise a `spawn Supervisor` (which would route
        // through the actor-spawn path the supervisor type does not have).
        if supervisor_layouts.contains_key(&child.ty) {
            continue;
        }

        // Determine whether any explicit init-arg NAME is invalid against the
        // actor's accepted field/init surface — WITHOUT emitting a
        // diagnostic here. The authoritative InvalidActorSpawnArgument for a
        // bad name is emitted earlier in the pipeline, in the post-loop
        // supervisor-layout pass in `lower_hir_module_with_facts` (S3), which
        // runs before this synthetic-body lowering and also skips its own
        // missing-field planning for the same child once a bad name is
        // found. Re-emitting here would duplicate that diagnostic; this
        // check exists solely so the spawn statement for an invalid child is
        // still skipped (never routed through the synthesized-body lowering
        // that assumes every arg name resolved).
        if let Some(actor_layout) = actor_layouts.get(&child.ty) {
            let explicit_init = actor_layout.init_symbol.is_some();
            let has_invalid_arg = child.init_args.iter().any(|(arg_name, _)| {
                let is_valid = if explicit_init {
                    actor_layout.init_param_names.iter().any(|n| n == arg_name)
                        || actor_layout.state_field_names.iter().any(|n| n == arg_name)
                } else {
                    actor_layout.state_field_names.iter().any(|n| n == arg_name)
                };
                !is_valid
            });
            if has_invalid_arg {
                continue;
            }
        }

        // A config-init child (any init arg reads `config.field`) is produced
        // entirely by the codegen init thunk, which emits the whole bootstrap
        // body from `SupervisorChildLayout` and DISCARDS this synthetic body.
        // Re-lowering a `config.field` read here would (a) serve no purpose (the
        // body is discarded) and (b) reference the bootstrap config param via a
        // BindingRef the synthetic body's dataflow can't resolve — tripping a
        // spurious "read before initialised" on an owned config field. Skip the
        // spawn statement for such a child entirely (like a nested supervisor),
        // leaving the synthetic body a valid signature carrier. Name validity
        // was already checked above, so nothing is lost by skipping.
        if child
            .init_args
            .iter()
            .any(|(_, expr)| hir_expr_reads_config_field(expr))
        {
            continue;
        }

        let handle_ty = local_pid_of(&child.ty);

        // Build the spawn's args vec from the wired_to map. We iterate
        // the child's wired_to map in a deterministic (sorted) order so
        // the emitted MIR is stable across runs.
        let mut spawn_args: Vec<(String, HirExpr)> = Vec::new();
        // Named init args: include literal values from the child declaration so
        // the synthesized spawn satisfies the per-field arity gate in
        // lower_spawn_actor_state. The bootstrap body is a stub that codegen
        // discards (see SHIM comment below); these args exist only to pass the
        // MIR completeness check. The real state template is built by codegen
        // from SupervisorChildLayout.init_state_fields.
        for (field_name, hir_expr) in &child.init_args {
            let arg_expr = HirExpr {
                node: fresh_node(),
                site: fresh_site(),
                ty: hir_expr.ty.clone(),
                value_class: hir_expr.value_class,
                intent: IntentKind::Read,
                kind: hir_expr.kind.clone(),
                span: sup.span.clone(),
            };
            spawn_args.push((field_name.clone(), arg_expr));
        }

        if let Some(wired) = &child.wired_to {
            let mut entries: Vec<(&String, &String)> = wired.iter().collect();
            entries.sort_by(|a, b| a.0.cmp(b.0));
            for (init_param, sibling_name) in entries {
                let Some((sibling_binding, sibling_ty)) = child_bindings.get(sibling_name).cloned()
                else {
                    // S-B should have rejected an unknown sibling; if we
                    // reach this branch the sibling either wasn't in the
                    // supervisor or appears after the dependent (which
                    // the topo sort just forbade). Either way, refuse to
                    // emit.
                    diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::NotYetImplemented {
                            construct: format!(
                                "supervisor `{}` child `{}` wired_to refers to sibling \
                                 `{sibling_name}` which is unresolved at spawn time",
                                sup.name, child.name
                            ),
                            site: SiteId(0),
                        },
                        note: "topological spawn order should make every wired sibling \
                               available before its dependent; if this fires the checker \
                               wired_to validation was bypassed"
                            .to_string(),
                    });
                    return None;
                };
                let arg_expr = HirExpr {
                    node: fresh_node(),
                    site: fresh_site(),
                    ty: sibling_ty,
                    value_class: ValueClass::BitCopy,
                    intent: IntentKind::Read,
                    kind: HirExprKind::BindingRef {
                        name: sibling_name.clone(),
                        resolved: ResolvedRef::Binding(sibling_binding),
                    },
                    span: sup.span.clone(),
                };
                spawn_args.push((init_param.clone(), arg_expr));
            }
        }

        let spawn_expr = HirExpr {
            node: fresh_node(),
            site: fresh_site(),
            ty: handle_ty.clone(),
            value_class: ValueClass::BitCopy,
            intent: IntentKind::Read,
            kind: HirExprKind::Spawn {
                actor_name: child.ty.clone(),
                args: spawn_args,
            },
            span: sup.span.clone(),
        };

        let binding = HirBinding {
            id: fresh_binding(),
            name: child.name.clone(),
            ty: handle_ty.clone(),
            mutable: false,
            span: sup.span.clone(),
            is_consume: false,
        };
        child_bindings.insert(child.name.clone(), (binding.id, handle_ty));

        statements.push(HirStmt {
            node: fresh_node(),
            kind: HirStmtKind::Let(binding, Some(spawn_expr)),
            span: sup.span.clone(),
        });
    }

    let body = HirBlock {
        node: fresh_node(),
        scope: ScopeId(0),
        statements,
        tail: None,
        ty: ResolvedTy::Unit,
        span: sup.span.clone(),
    };

    // Bootstrap returns `LocalPid<Sup>` so callers of `spawn Sup` receive a
    // typed handle. The body does not yet emit a return value — S-D.3 replaces
    // the body wholesale with the `hew_supervisor_*` call sequence and a real
    // return. For this slice the return slot is intentionally left unwritten;
    // the MIR-level dataflow checker does not enforce return-slot initialisation,
    // and codegen will never see this stub body (S-D.3 lands before codegen
    // is exercised on supervisor programs).
    //
    // SHIM(S-D.1→S-D.3): bootstrap body is a stub. Return slot unwritten.
    // WHY: type signature must be correct before the routing call site is
    //   wired; body replacement is a separate slice.
    // WHEN obsolete: S-D.3 replaces the body with the hew_supervisor_* sequence.
    // WHAT: emit hew_supervisor_start/register_child/etc in S-D.3.
    // The bootstrap fn carries the supervisor's config param (when declared) as
    // its sole parameter. Codegen's bootstrap-body emitter reads it (the config
    // struct passed by value) to malloc the supervisor-owned config buffer and
    // thread it to each config-init child's init thunk. The `spawn App(cfg)`
    // call site (lower_spawn_actor) passes the config value as this arg, so the
    // declared signature and the call must agree — both derive from this param.
    // No config param → an empty param list (the no-config supervisor).
    let bootstrap_params: Vec<HirBinding> = sup
        .params
        .first()
        .map(|param| {
            vec![HirBinding {
                id: fresh_binding(),
                name: param.name.clone(),
                ty: param.ty.clone(),
                mutable: false,
                span: sup.span.clone(),
                is_consume: false,
            }]
        })
        .unwrap_or_default();

    let synthetic_fn = HirFn {
        id: sup.id,
        node: sup.node,
        name: format!("{}::__bootstrap", sup.name),
        type_params: Vec::new(),
        params: bootstrap_params,
        return_ty: local_pid_of(&sup.name),
        body,
        span: sup.span.clone(),
        is_generator: false,
        intrinsic_id: None,
    };

    Some(lower_function(
        &synthetic_fn,
        emit_name,
        HashMap::new(),
        type_classes,
        record_field_orders,
        actor_layouts,
        supervisor_layouts,
        machine_layout_names,
        enum_layouts,
        opaque_handle_names,
        // Supervisors have no actor state. `lower_actor_init_handler`
        // passes `Some(&actor.name)` for the same role; here we pass
        // `None` because there's no state-field table to lift into the
        // Builder.
        None,
        module_fn_names,
        module_generic_fn_names,
        funcupdate_fn_returns_fresh,
        call_scrutinee_provenance,
        param_ownership,
        &HashMap::new(),
        call_site_type_args,
        None,
        supervisor_child_slots,
        pool_accessor_sites,
        actor_send_aliasing,
        pointer_width,
        // `FunctionCallConv::Default`: codegen replaces the bootstrap body
        // wholesale with the `hew_supervisor_*` call sequence (S-D.3), so
        // the body never reads an execution context. The synthetic call
        // site at `lower_spawn` emits `Terminator::Call { args: [] }`; an
        // `ActorHandler` declaration would add a leading ctx LLVM parameter
        // that the caller does not supply, tripping `llvm.verify`. The
        // doc-comment on `mangle_supervisor_bootstrap` describing the
        // supervisor as actor-like still applies at the runtime level — the
        // supervisor's own actor is spawned by `hew_supervisor_start`, not
        // by the bootstrap's call_conv.
        crate::model::FunctionCallConv::Default,
        Rc::clone(task_entry_adapter_symbols),
    ))
}
/// Build the MIR `ActorHandlerLayout` row for every `receive fn` on this
/// actor, drawing the `msg_type` from the checker's protocol descriptor.
///
/// Pre-Q87 this iterated `receive_handlers.iter().enumerate()` and used the
/// source-order index as the wire-format `msg_id` — which silently flipped
/// the ABI on every handler reorder. Q87 slice 1 replaces that with the
/// stable, hash-derived id from
/// `HirActorDecl::protocol_descriptor`. Source-order rearrangement no
/// longer affects the protocol.
///
/// Fail-closed: an actor that carries `receive_handlers` but no descriptor
/// (the checker emitted an `ActorProtocolCollision` and refused to publish
/// the protocol, a lowering path failed to attach the checker's descriptor,
/// or an upstream bug) falls back to `i32::MAX` here — but the layout pass
/// pairs that fallback with a hard `ActorProtocolDescriptorMissing`
/// diagnostic (see the guard before the `ActorLayout` push in
/// `lower_hir_module`), so no compiled program ever dispatches on the
/// sentinel. The sentinel rows exist only to keep the MIR shape well-formed
/// behind that hard error.
pub(super) fn lower_actor_handler_layouts(actor: &HirActorDecl) -> Vec<ActorHandlerLayout> {
    let descriptor = actor.protocol_descriptor.as_ref();
    let mut layouts = Vec::with_capacity(actor.receive_handlers.len());
    for handler in &actor.receive_handlers {
        // The checker is the only authority for state-guard facts.
        // `HirActorStateGuard` is intentionally closed at one variant; any
        // future variant addition is a compile error here and must pair
        // with a policy decision in this match. Populated for a generator
        // handler exactly like any other (`lower_actor_receive_fn` computes
        // it unconditionally).
        let requires_state_guard = match handler.state_guard {
            hew_hir::HirActorStateGuard::Exclusive => true,
        };
        let msg_type = descriptor
            .and_then(|d| d.msg_id_for(&handler.name))
            .map_or(i32::MAX, |id| i32::from_ne_bytes(id.to_ne_bytes()));

        if handler.is_generator {
            // A `receive gen fn` IS a message-dispatch handler — the third
            // handler kind beside tell (`Fire`) and ask (`Ask`): a per-call
            // stream-producer PUMP, started by a tell-shaped "start" message
            // (decision 4; the checker/HIR/MIR call-site bridge constructs
            // and sends that message). `param_tys`
            // mirrors the shell's actual MIR signature built in
            // `lower_actor_receive_handlers`: the handler's own params plus
            // one trailing pointer-word sink — this row IS the single
            // pack/unpack authority for the start message on both the
            // trampoline (unpack) and the future call-site (pack) ends.
            // `every_ms` stays `None`: a generator handler cannot be
            // periodic (Risk 6 — `#[every]` on `receive gen fn` is a
            // checker/HIR-level reject, not a layout concern). `return_ty`
            // is `Unit`, matching the pump's actual MIR return type — the
            // stream element type lives on the checker's
            // `ActorMethodKind::StreamProducer` fact and the HIR
            // `ActorGenStream` expression, not this row.
            let mut param_tys: Vec<ResolvedTy> = handler
                .params
                .iter()
                .map(|param| param.ty.clone())
                .collect();
            param_tys.push(ResolvedTy::named_builtin(
                "Sink",
                hew_types::BuiltinType::Sink,
                vec![handler.return_ty.clone()],
            ));
            layouts.push(ActorHandlerLayout {
                name: handler.name.clone(),
                symbol: mangle_actor_receive_handler(&actor_symbol_base(actor), &handler.name),
                msg_type,
                every_ms: None,
                param_tys,
                return_ty: ResolvedTy::Unit,
                requires_state_guard,
                is_stream_producer: true,
            });
            continue;
        }

        // ns→ms truncating divide — the runtime timer ABI
        // (`hew_actor_schedule_periodic`) is millisecond-grained. The
        // checker rejects sub-millisecond intervals, so 0 is unreachable
        // for checked programs; an unchecked 0 is refused by the runtime
        // (null handle) and trapped by codegen's spawn-site check.
        let every_ms = handler
            .every_ns
            .map(|ns| u64::try_from(ns).map_or(0, |ns| ns / 1_000_000));
        layouts.push(ActorHandlerLayout {
            name: handler.name.clone(),
            symbol: mangle_actor_receive_handler(&actor_symbol_base(actor), &handler.name),
            msg_type,
            every_ms,
            param_tys: handler
                .params
                .iter()
                .map(|param| param.ty.clone())
                .collect(),
            return_ty: handler.return_ty.clone(),
            requires_state_guard,
            is_stream_producer: false,
        });
    }
    layouts
}
fn unknown_self_fields_in_block(block: &HirBlock, state_fields: &HashSet<String>) -> Vec<String> {
    let mut seen = HashSet::new();
    let mut unknown = Vec::new();
    collect_unknown_self_fields_in_block(block, state_fields, &mut seen, &mut unknown);
    unknown
}
fn collect_unknown_self_fields_in_block(
    block: &HirBlock,
    state_fields: &HashSet<String>,
    seen: &mut HashSet<String>,
    unknown: &mut Vec<String>,
) {
    for stmt in &block.statements {
        match &stmt.kind {
            HirStmtKind::Let(_, Some(expr))
            | HirStmtKind::Expr(expr)
            | HirStmtKind::Return(Some(expr)) => {
                collect_unknown_self_fields_in_expr(expr, state_fields, seen, unknown);
            }
            HirStmtKind::Assign { target, value } => {
                collect_unknown_self_fields_in_expr(target, state_fields, seen, unknown);
                collect_unknown_self_fields_in_expr(value, state_fields, seen, unknown);
            }
            HirStmtKind::Let(_, None) | HirStmtKind::Return(None) => {}
            HirStmtKind::Defer { body, .. } => {
                collect_unknown_self_fields_in_expr(body, state_fields, seen, unknown);
            }
            HirStmtKind::LetElse {
                scrutinee,
                success_prelude,
                else_body,
                ..
            } => {
                collect_unknown_self_fields_in_expr(scrutinee, state_fields, seen, unknown);
                for prelude_stmt in success_prelude {
                    if let HirStmtKind::Let(_, Some(value)) = &prelude_stmt.kind {
                        collect_unknown_self_fields_in_expr(value, state_fields, seen, unknown);
                    }
                }
                collect_unknown_self_fields_in_block(else_body, state_fields, seen, unknown);
            }
        }
    }
    if let Some(tail) = &block.tail {
        collect_unknown_self_fields_in_expr(tail, state_fields, seen, unknown);
    }
}
#[allow(
    clippy::too_many_lines,
    reason = "visitor mirrors the sealed HirExprKind surface so self-field validation is exhaustive"
)]
fn collect_unknown_self_fields_in_expr(
    expr: &HirExpr,
    state_fields: &HashSet<String>,
    seen: &mut HashSet<String>,
    unknown: &mut Vec<String>,
) {
    match &expr.kind {
        HirExprKind::Literal(_)
        | HirExprKind::RegexLiteralRef { .. }
        | HirExprKind::BindingRef { .. }
        | HirExprKind::AwaitTask { .. }
        | HirExprKind::ContextReader { .. }
        | HirExprKind::MachineFieldAccess { .. }
        | HirExprKind::MachineEventFieldAccess { .. }
        | HirExprKind::Continue { .. }
        | HirExprKind::ActorSelf
        | HirExprKind::Unsupported(_) => {}
        HirExprKind::Binary { left, right, .. } | HirExprKind::IdentityCompare { left, right } => {
            collect_unknown_self_fields_in_expr(left, state_fields, seen, unknown);
            collect_unknown_self_fields_in_expr(right, state_fields, seen, unknown);
        }
        HirExprKind::Unary { operand, .. } | HirExprKind::WireCodec { operand, .. } => {
            collect_unknown_self_fields_in_expr(operand, state_fields, seen, unknown);
        }
        HirExprKind::ConnAwaitRead { conn, .. } => {
            collect_unknown_self_fields_in_expr(conn, state_fields, seen, unknown);
        }
        HirExprKind::AwaitRestart { child } => {
            collect_unknown_self_fields_in_expr(child, state_fields, seen, unknown);
        }
        HirExprKind::ListenerAwaitAccept { listener, .. } => {
            collect_unknown_self_fields_in_expr(listener, state_fields, seen, unknown);
        }
        HirExprKind::StreamRecvAwait { stream, .. } => {
            collect_unknown_self_fields_in_expr(stream, state_fields, seen, unknown);
        }
        HirExprKind::NumericCast { value, .. }
        | HirExprKind::SaturatingWidthCast { value, .. }
        | HirExprKind::TryWidthCast { value, .. }
        | HirExprKind::CoerceToDynTrait { value, .. } => {
            collect_unknown_self_fields_in_expr(value, state_fields, seen, unknown);
        }
        HirExprKind::TupleLiteral { elements } => {
            for elem in elements {
                collect_unknown_self_fields_in_expr(elem, state_fields, seen, unknown);
            }
        }
        HirExprKind::NumericMethod { receiver, arg, .. } => {
            collect_unknown_self_fields_in_expr(receiver, state_fields, seen, unknown);
            collect_unknown_self_fields_in_expr(arg, state_fields, seen, unknown);
        }
        HirExprKind::Call { callee, args } | HirExprKind::SpawnedCall { callee, args, .. } => {
            collect_unknown_self_fields_in_expr(callee, state_fields, seen, unknown);
            for arg in args {
                collect_unknown_self_fields_in_expr(arg, state_fields, seen, unknown);
            }
        }
        HirExprKind::Spawn { args, .. } => {
            for (_, arg) in args {
                collect_unknown_self_fields_in_expr(arg, state_fields, seen, unknown);
            }
        }
        HirExprKind::ActorSend { receiver, args, .. }
        | HirExprKind::ActorAsk { receiver, args, .. }
        | HirExprKind::ActorGenStream { receiver, args, .. }
        | HirExprKind::CallDynMethod { receiver, args, .. }
        | HirExprKind::ResolvedImplCall { receiver, args, .. }
        | HirExprKind::CallTraitMethodStatic { receiver, args, .. }
        | HirExprKind::VarSelfMethodCall { receiver, args, .. } => {
            collect_unknown_self_fields_in_expr(receiver, state_fields, seen, unknown);
            for arg in args {
                collect_unknown_self_fields_in_expr(arg, state_fields, seen, unknown);
            }
        }
        HirExprKind::RemoteActorAsk {
            receiver,
            msg,
            timeout_ms,
            ..
        } => {
            collect_unknown_self_fields_in_expr(receiver, state_fields, seen, unknown);
            collect_unknown_self_fields_in_expr(msg, state_fields, seen, unknown);
            collect_unknown_self_fields_in_expr(timeout_ms, state_fields, seen, unknown);
        }
        HirExprKind::Block(block)
        | HirExprKind::Scope { body: block }
        | HirExprKind::ForkBlock { body: block, .. }
        | HirExprKind::GenBlock { body: block, .. } => {
            collect_unknown_self_fields_in_block(block, state_fields, seen, unknown);
        }
        HirExprKind::Yield { value, .. }
        | HirExprKind::Break { value, .. }
        | HirExprKind::Return { value } => {
            if let Some(value) = value {
                collect_unknown_self_fields_in_expr(value, state_fields, seen, unknown);
            }
        }
        HirExprKind::If {
            condition,
            then_expr,
            else_expr,
        } => {
            collect_unknown_self_fields_in_expr(condition, state_fields, seen, unknown);
            collect_unknown_self_fields_in_expr(then_expr, state_fields, seen, unknown);
            if let Some(else_expr) = else_expr {
                collect_unknown_self_fields_in_expr(else_expr, state_fields, seen, unknown);
            }
        }
        HirExprKind::StructInit { fields, base, .. } => {
            for (_, field_expr) in fields {
                collect_unknown_self_fields_in_expr(field_expr, state_fields, seen, unknown);
            }
            if let Some(base) = base {
                collect_unknown_self_fields_in_expr(base, state_fields, seen, unknown);
            }
        }
        HirExprKind::FieldAccess { object, field } => {
            if matches!(
                &object.kind,
                HirExprKind::BindingRef {
                    name,
                    resolved: ResolvedRef::Unresolved | ResolvedRef::Binding(_) | ResolvedRef::Item(_)
                } if name == "self"
            ) && !state_fields.contains(field)
                && seen.insert(field.clone())
            {
                unknown.push(field.clone());
            }
            collect_unknown_self_fields_in_expr(object, state_fields, seen, unknown);
        }
        HirExprKind::ScopeDeadline { duration, body } => {
            collect_unknown_self_fields_in_expr(duration, state_fields, seen, unknown);
            collect_unknown_self_fields_in_block(body, state_fields, seen, unknown);
        }
        HirExprKind::Select(select) => {
            for arm in &select.arms {
                match &arm.kind {
                    hew_hir::HirSelectArmKind::StreamNext { stream } => {
                        collect_unknown_self_fields_in_expr(stream, state_fields, seen, unknown);
                    }
                    hew_hir::HirSelectArmKind::ActorAsk { actor, args, .. } => {
                        collect_unknown_self_fields_in_expr(actor, state_fields, seen, unknown);
                        for arg in args {
                            collect_unknown_self_fields_in_expr(arg, state_fields, seen, unknown);
                        }
                    }
                    hew_hir::HirSelectArmKind::TaskAwait { task } => {
                        collect_unknown_self_fields_in_expr(task, state_fields, seen, unknown);
                    }
                    hew_hir::HirSelectArmKind::ChannelRecv { receiver, .. } => {
                        collect_unknown_self_fields_in_expr(receiver, state_fields, seen, unknown);
                    }
                    hew_hir::HirSelectArmKind::AfterTimer { duration } => {
                        collect_unknown_self_fields_in_expr(duration, state_fields, seen, unknown);
                    }
                }
                collect_unknown_self_fields_in_expr(&arm.body, state_fields, seen, unknown);
            }
        }
        HirExprKind::Join(join) => {
            for branch in &join.branches {
                collect_unknown_self_fields_in_expr(&branch.actor, state_fields, seen, unknown);
                for arg in &branch.args {
                    collect_unknown_self_fields_in_expr(arg, state_fields, seen, unknown);
                }
            }
        }
        HirExprKind::SpawnLambdaActor { body, .. } | HirExprKind::Closure { body, .. } => {
            collect_unknown_self_fields_in_expr(body, state_fields, seen, unknown);
        }
        HirExprKind::TupleIndex { tuple, .. } => {
            collect_unknown_self_fields_in_expr(tuple, state_fields, seen, unknown);
        }
        HirExprKind::Index { container, index } => {
            collect_unknown_self_fields_in_expr(container, state_fields, seen, unknown);
            collect_unknown_self_fields_in_expr(index, state_fields, seen, unknown);
        }
        HirExprKind::Slice {
            container,
            start,
            end,
            ..
        } => {
            collect_unknown_self_fields_in_expr(container, state_fields, seen, unknown);
            if let Some(start) = start {
                collect_unknown_self_fields_in_expr(start, state_fields, seen, unknown);
            }
            if let Some(end) = end {
                collect_unknown_self_fields_in_expr(end, state_fields, seen, unknown);
            }
        }
        HirExprKind::MachineEmit { fields, .. } => {
            for (_, field_val) in fields {
                collect_unknown_self_fields_in_expr(field_val, state_fields, seen, unknown);
            }
        }
        HirExprKind::MachineStep {
            receiver, event, ..
        }
        | HirExprKind::MachineTakeEmits {
            receiver, event, ..
        } => {
            collect_unknown_self_fields_in_expr(receiver, state_fields, seen, unknown);
            collect_unknown_self_fields_in_expr(event, state_fields, seen, unknown);
        }
        HirExprKind::RcIntrinsic {
            receiver, value, ..
        } => {
            if let Some(receiver) = receiver {
                collect_unknown_self_fields_in_expr(receiver, state_fields, seen, unknown);
            }
            if let Some(value) = value {
                collect_unknown_self_fields_in_expr(value, state_fields, seen, unknown);
            }
        }
        HirExprKind::ChannelRecvAwait { receiver, .. }
        | HirExprKind::CancellationTokenIsCancelled { receiver }
        | HirExprKind::GeneratorNext { receiver, .. }
        | HirExprKind::MachineStateName { receiver, .. }
        | HirExprKind::RecordCloneCall { src: receiver, .. } => {
            collect_unknown_self_fields_in_expr(receiver, state_fields, seen, unknown);
        }
        HirExprKind::MachineVariantCtor { payload, .. } => {
            if let Some(fields) = payload {
                for (_, val) in fields {
                    collect_unknown_self_fields_in_expr(val, state_fields, seen, unknown);
                }
            }
        }
        HirExprKind::While {
            condition, body, ..
        } => {
            collect_unknown_self_fields_in_expr(condition, state_fields, seen, unknown);
            collect_unknown_self_fields_in_block(body, state_fields, seen, unknown);
        }
        HirExprKind::ForRange {
            start,
            end,
            step,
            body,
            ..
        } => {
            collect_unknown_self_fields_in_expr(start, state_fields, seen, unknown);
            collect_unknown_self_fields_in_expr(end, state_fields, seen, unknown);
            collect_unknown_self_fields_in_expr(step, state_fields, seen, unknown);
            collect_unknown_self_fields_in_block(body, state_fields, seen, unknown);
        }
        HirExprKind::Match { scrutinee, arms } => {
            collect_unknown_self_fields_in_expr(scrutinee, state_fields, seen, unknown);
            for arm in arms {
                collect_unknown_self_fields_in_expr(&arm.body, state_fields, seen, unknown);
            }
        }
        HirExprKind::WhileLet {
            scrutinee, body, ..
        } => {
            collect_unknown_self_fields_in_expr(scrutinee, state_fields, seen, unknown);
            collect_unknown_self_fields_in_block(body, state_fields, seen, unknown);
        }
        HirExprKind::IfLet {
            scrutinee,
            body,
            else_body,
            ..
        } => {
            collect_unknown_self_fields_in_expr(scrutinee, state_fields, seen, unknown);
            collect_unknown_self_fields_in_block(body, state_fields, seen, unknown);
            if let Some(eb) = else_body {
                collect_unknown_self_fields_in_block(eb, state_fields, seen, unknown);
            }
        }
        HirExprKind::Loop { body, .. } => {
            collect_unknown_self_fields_in_block(body, state_fields, seen, unknown);
        }
    }
}
