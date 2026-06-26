use std::collections::HashMap;

use hew_hir::{
    ids::IdGen, HirActorDecl, HirActorReceiveFn, HirActorStateGuard, HirBinding, HirBlock, HirExpr,
    HirExprKind, HirField, HirFn, HirItem, HirLifecycleHook, HirLifecycleHookKind, HirLiteral,
    HirModule, HirStmt, HirStmtKind, HirSupervisorChild, HirSupervisorDecl, IntentKind,
    ResolvedRef, ScopeId, ValueClass,
};
use hew_mir::{lower_hir_module, FunctionCallConv, Instr, MirDiagnosticKind, Terminator};
use hew_types::{ActorHandlerSpec, ActorProtocolDescriptor, BuiltinType, ResolvedTy};

fn empty_module(items: Vec<HirItem>) -> HirModule {
    HirModule {
        items,
        diagnostic_source_modules: HashMap::default(),
        wire_layouts: std::sync::Arc::new(HashMap::default()),
        type_classes: HashMap::default(),
        monomorphisations: vec![],
        call_site_type_args: HashMap::default(),
        vec_generic_element_abi: HashMap::default(),
        record_layouts: vec![],
        enum_layouts: vec![],
        machine_instantiations: vec![],
        supervisor_child_slots: HashMap::default(),
        regex_literals: vec![],
    }
}

fn binding(ids: &mut IdGen, name: &str, ty: ResolvedTy) -> HirBinding {
    HirBinding {
        id: ids.binding(),
        name: name.to_string(),
        ty,
        mutable: false,
        span: 0..0,
    }
}

fn block(
    ids: &mut IdGen,
    statements: Vec<HirStmt>,
    tail: Option<HirExpr>,
    ty: ResolvedTy,
) -> HirBlock {
    HirBlock {
        node: ids.node(),
        scope: ScopeId(0),
        statements,
        tail: tail.map(Box::new),
        ty,
        span: 0..0,
    }
}

fn return_none_stmt(ids: &mut IdGen) -> HirStmt {
    HirStmt {
        node: ids.node(),
        kind: HirStmtKind::Return(None),
        span: 0..0,
    }
}

fn literal_expr(ids: &mut IdGen, literal: HirLiteral, ty: ResolvedTy) -> HirExpr {
    HirExpr {
        node: ids.node(),
        site: ids.site(),
        ty,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::Literal(literal),
        span: 0..0,
    }
}

fn local_pid_of(actor_name: &str) -> ResolvedTy {
    ResolvedTy::named_builtin(
        "LocalPid",
        BuiltinType::LocalPid,
        vec![ResolvedTy::named_user(actor_name, vec![])],
    )
}

fn self_field_expr(ids: &mut IdGen, actor_name: &str, field: &str) -> HirExpr {
    let self_expr = HirExpr {
        node: ids.node(),
        site: ids.site(),
        ty: ResolvedTy::Named {
            name: actor_name.to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        },
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::BindingRef {
            name: "self".to_string(),
            resolved: ResolvedRef::Unresolved,
        },
        span: 0..0,
    };
    HirExpr {
        node: ids.node(),
        site: ids.site(),
        ty: ResolvedTy::I64,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::FieldAccess {
            object: Box::new(self_expr),
            field: field.to_string(),
        },
        span: 0..0,
    }
}

fn spawn_expr(
    ids: &mut IdGen,
    actor_name: &str,
    args: Vec<(String, HirExpr)>,
    ty: ResolvedTy,
) -> HirExpr {
    HirExpr {
        node: ids.node(),
        site: ids.site(),
        ty,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::Spawn {
            actor_name: actor_name.to_string(),
            args,
        },
        span: 0..0,
    }
}

fn receive(
    name: &str,
    is_generator: bool,
    params: Vec<HirBinding>,
    return_ty: ResolvedTy,
    body: HirBlock,
) -> HirActorReceiveFn {
    HirActorReceiveFn {
        name: name.to_string(),
        is_generator,
        params,
        return_ty,
        body,
        state_guard: HirActorStateGuard::Exclusive,
        every_ns: None,
        span: 0..0,
    }
}

fn actor(ids: &mut IdGen, name: &str, receive_handlers: Vec<HirActorReceiveFn>) -> HirActorDecl {
    let init_return = return_none_stmt(ids);
    let init_body = block(ids, vec![init_return], None, ResolvedTy::Unit);
    let init_param = binding(ids, "initial", ResolvedTy::I64);
    // Q87 slice 1: synthesise a descriptor from the receive-handler list so
    // MIR's `msg_type` derivation succeeds with the default hash contract
    // instead of falling back to the sentinel. Tests in this file do not
    // assert on `msg_type` values, but a hand-built actor with multiple
    // handlers must not all collide on `i32::MAX`.
    let protocol_descriptor = if receive_handlers.is_empty() {
        None
    } else {
        let specs: Vec<ActorHandlerSpec> = receive_handlers
            .iter()
            .map(|h| ActorHandlerSpec {
                name: h.name.clone(),
                param_tys: h.params.iter().map(|p| p.ty.clone()).collect(),
                return_ty: h.return_ty.clone(),
                symbol: format!("{name}__{}", h.name),
            })
            .collect();
        Some(
            ActorProtocolDescriptor::from_handlers(name.to_string(), &specs)
                .expect("test-built actor handler names must not collide"),
        )
    };
    HirActorDecl {
        id: ids.item(),
        node: ids.node(),
        name: name.to_string(),
        defining_module: None,
        state_fields: vec![HirField {
            name: "count".to_string(),
            ty: ResolvedTy::I64,
            default: None,
            is_mutable: true,
            span: 0..0,
        }],
        init: Some(hew_hir::HirActorInit {
            params: vec![init_param],
            body: init_body,
        }),
        receive_handlers,
        methods: vec![],
        lifecycle_hooks: vec![],
        max_heap_bytes: None,
        is_isolated: false,
        mailbox_capacity: None,
        overflow_policy: None,
        cycle_capable: false,
        protocol_descriptor,
        span: 0..0,
    }
}

#[test]
fn actor_cycle_capable_threads_to_layout_and_spawn_instr() {
    let mut ids = IdGen::default();
    let mut actor = actor(&mut ids, "Counter", vec![]);
    actor.cycle_capable = true;

    let pid_ty = local_pid_of("Counter");
    let initial = literal_expr(&mut ids, HirLiteral::Integer(0), ResolvedTy::I64);
    let spawn = spawn_expr(
        &mut ids,
        "Counter",
        vec![("initial".to_string(), initial)],
        pid_ty.clone(),
    );
    let main = HirFn {
        id: ids.item(),
        node: ids.node(),
        name: "main".to_string(),
        type_params: vec![],
        params: vec![],
        return_ty: pid_ty,
        body: block(&mut ids, vec![], Some(spawn), local_pid_of("Counter")),
        span: 0..0,
        is_generator: false,
        intrinsic_id: None,
    };

    let pipeline = lower_hir_module(&empty_module(vec![
        HirItem::Actor(actor),
        HirItem::Function(main),
    ]));

    assert!(
        pipeline.diagnostics.is_empty(),
        "cycle-capable actor spawn should lower without diagnostics: {:?}",
        pipeline.diagnostics
    );
    assert_eq!(pipeline.actor_layouts.len(), 1);
    assert!(
        pipeline.actor_layouts[0].cycle_capable,
        "ActorLayout must preserve HirActorDecl.cycle_capable"
    );
    let main = pipeline
        .raw_mir
        .iter()
        .find(|func| func.name == "main")
        .expect("main lowered");
    assert!(main
        .blocks
        .iter()
        .flat_map(|b| &b.instructions)
        .any(|instr| {
            matches!(
                instr,
                Instr::SpawnActor {
                    actor_name,
                    cycle_capable: true,
                    ..
                } if actor_name == "Counter"
            )
        }));
}

#[test]
fn non_cycle_actor_keeps_false_layout_and_spawn_default() {
    let mut ids = IdGen::default();
    let actor = actor(&mut ids, "Counter", vec![]);

    let pid_ty = local_pid_of("Counter");
    let initial = literal_expr(&mut ids, HirLiteral::Integer(0), ResolvedTy::I64);
    let spawn = spawn_expr(
        &mut ids,
        "Counter",
        vec![("initial".to_string(), initial)],
        pid_ty.clone(),
    );
    let main = HirFn {
        id: ids.item(),
        node: ids.node(),
        name: "main".to_string(),
        type_params: vec![],
        params: vec![],
        return_ty: pid_ty,
        body: block(&mut ids, vec![], Some(spawn), local_pid_of("Counter")),
        span: 0..0,
        is_generator: false,
        intrinsic_id: None,
    };

    let pipeline = lower_hir_module(&empty_module(vec![
        HirItem::Actor(actor),
        HirItem::Function(main),
    ]));

    assert!(
        pipeline.diagnostics.is_empty(),
        "non-cycle actor spawn should lower without diagnostics: {:?}",
        pipeline.diagnostics
    );
    assert!(!pipeline.actor_layouts[0].cycle_capable);
    let main = pipeline
        .raw_mir
        .iter()
        .find(|func| func.name == "main")
        .expect("main lowered");
    assert!(main
        .blocks
        .iter()
        .flat_map(|b| &b.instructions)
        .any(|instr| {
            matches!(
                instr,
                Instr::SpawnActor {
                    actor_name,
                    cycle_capable: false,
                    ..
                } if actor_name == "Counter"
            )
        }));
}

/// The defining-module identity carried on `HirActorDecl` must survive the
/// HIR→MIR boundary into `ActorLayout`. The layout REGISTRY key is the
/// dotted qualified identity (`bank.Account`) and every native symbol
/// derives from the `$`-mangled base (`bank$Account`), so two same-named
/// module actors occupy distinct entries AND distinct LLVM symbols —
/// producer (MIR mangle) and consumer (codegen `get_function`) flip
/// together through the single `actor_symbol_base` authority.
#[test]
fn defining_module_survives_to_actor_layout_with_qualified_key() {
    let mut ids = IdGen::default();
    let bump_body = block(&mut ids, vec![], None, ResolvedTy::Unit);
    let mut imported = actor(
        &mut ids,
        "Account",
        vec![receive("bump", false, vec![], ResolvedTy::Unit, bump_body)],
    );
    imported.defining_module = Some("bank".to_string());

    let pipeline = lower_hir_module(&empty_module(vec![HirItem::Actor(imported)]));

    assert!(
        pipeline.diagnostics.is_empty(),
        "carrier-only actor should lower without diagnostics: {:?}",
        pipeline.diagnostics
    );
    let layout = &pipeline.actor_layouts[0];
    assert_eq!(
        layout.defining_module.as_deref(),
        Some("bank"),
        "ActorLayout must preserve HirActorDecl.defining_module"
    );
    // Registry key dotted; symbols `$`-mangled from the same identity.
    assert_eq!(layout.name, "bank.Account");
    assert_eq!(
        layout.state_clone_fn_symbol.as_deref(),
        Some("__hew_state_clone_bank$Account")
    );
    assert_eq!(
        layout.state_drop_fn_symbol.as_deref(),
        Some("__hew_state_drop_bank$Account")
    );
    assert_eq!(layout.init_symbol.as_deref(), Some("bank$Account__init"));
    assert_eq!(layout.handlers[0].symbol, "bank$Account__recv__bump");
}

/// A root-program actor (no defining module) lowers with `None` in the
/// layout — the bare/root identity the qualified key maps to itself.
#[test]
fn root_actor_layout_carries_no_defining_module() {
    let mut ids = IdGen::default();
    let root = actor(&mut ids, "Counter", vec![]);

    let pipeline = lower_hir_module(&empty_module(vec![HirItem::Actor(root)]));

    assert!(
        pipeline.diagnostics.is_empty(),
        "root actor should lower without diagnostics: {:?}",
        pipeline.diagnostics
    );
    assert_eq!(pipeline.actor_layouts[0].defining_module, None);
}

#[test]
fn supervisor_child_layout_mirrors_cycle_capable_actor_metadata() {
    let mut ids = IdGen::default();
    let mut worker = actor(&mut ids, "Worker", vec![]);
    worker.state_fields.clear();
    worker.init = None;
    worker.cycle_capable = true;

    let sup = HirSupervisorDecl {
        id: ids.item(),
        node: ids.node(),
        name: "App".to_string(),
        params: Vec::new(),
        strategy: None,
        max_restarts: None,
        window: None,
        children: vec![HirSupervisorChild {
            name: "worker".to_string(),
            ty: "Worker".to_string(),
            restart_policy: None,
            wired_to: None,
            is_pool: false,
            slot_index: 0,
            init_args: Vec::new(),
            pool_count: None,
            shutdown: None,
        }],
        span: 0..0,
    };

    // Put the supervisor before the actor to exercise the post-loop mirror.
    let pipeline = lower_hir_module(&empty_module(vec![
        HirItem::Supervisor(sup),
        HirItem::Actor(worker),
    ]));

    assert!(
        pipeline.diagnostics.is_empty(),
        "supervisor child metadata mirror should not emit diagnostics: {:?}",
        pipeline.diagnostics
    );
    let child = &pipeline.supervisor_layouts[0].children[0];
    assert!(
        child.cycle_capable,
        "SupervisorChildLayout must mirror the child's ActorLayout.cycle_capable"
    );
}

#[test]
fn actor_receive_handlers_emit_actor_handler_functions_and_layout() {
    let mut ids = IdGen::default();
    let increment_return = return_none_stmt(&mut ids);
    let increment_body = block(&mut ids, vec![increment_return], None, ResolvedTy::Unit);
    let current_tail = literal_expr(&mut ids, HirLiteral::Integer(7), ResolvedTy::I64);
    let current_body = block(&mut ids, vec![], Some(current_tail), ResolvedTy::I64);
    let amount = binding(&mut ids, "amount", ResolvedTy::I64);
    let increment = receive(
        "increment",
        false,
        vec![amount],
        ResolvedTy::Unit,
        increment_body,
    );
    let current = receive("current", false, vec![], ResolvedTy::I64, current_body);
    let actor = actor(&mut ids, "Counter", vec![increment, current]);

    let pipeline = lower_hir_module(&empty_module(vec![HirItem::Actor(actor)]));

    assert!(
        pipeline.diagnostics.is_empty(),
        "happy-path actor handlers should not produce diagnostics: {:?}",
        pipeline.diagnostics
    );
    assert_eq!(pipeline.actor_layouts.len(), 1);
    assert_eq!(pipeline.actor_layouts[0].name, "Counter");
    assert_eq!(
        pipeline.actor_layouts[0].state_field_tys,
        vec![ResolvedTy::I64]
    );
    assert_eq!(
        pipeline.actor_layouts[0].init_param_names,
        vec!["initial".to_string()]
    );
    assert_eq!(
        pipeline.actor_layouts[0].init_param_tys,
        vec![ResolvedTy::I64]
    );
    assert_eq!(
        pipeline.actor_layouts[0].init_symbol.as_deref(),
        Some("Counter__init")
    );

    for symbol in [
        "Counter__init",
        "Counter__recv__increment",
        "Counter__recv__current",
    ] {
        let func = pipeline
            .raw_mir
            .iter()
            .find(|func| func.name == symbol)
            .unwrap_or_else(|| panic!("missing actor handler MIR function `{symbol}`"));
        assert_eq!(func.call_conv, FunctionCallConv::ActorHandler);
        assert!(matches!(
            func.blocks[0].instructions.first(),
            Some(Instr::EnterContext)
        ));
        for block in &func.blocks {
            if matches!(
                block.terminator,
                Terminator::Return | Terminator::Trap { .. }
            ) {
                assert!(
                    matches!(block.instructions.last(), Some(Instr::ExitContext)),
                    "terminal block {} in `{symbol}` must exit context: {:?}",
                    block.id,
                    block.instructions
                );
            }
        }
    }
}

/// `#[every]` carry: the HIR `every_ns` nanosecond annotation must surface on
/// the MIR `ActorHandlerLayout` as a millisecond interval, alongside the
/// descriptor-derived `msg_type`, so spawn-site codegen can arm the periodic
/// timer with the same message id the send path uses.
#[test]
fn actor_handler_every_ns_lowers_to_layout_every_ms() {
    let mut ids = IdGen::default();
    let tick_return = return_none_stmt(&mut ids);
    let tick_body = block(&mut ids, vec![tick_return], None, ResolvedTy::Unit);
    let mut tick = receive("tick", false, vec![], ResolvedTy::Unit, tick_body);
    tick.every_ns = Some(50_000_000); // 50ms
    let bump_return = return_none_stmt(&mut ids);
    let bump_body = block(&mut ids, vec![bump_return], None, ResolvedTy::Unit);
    let bump = receive("bump", false, vec![], ResolvedTy::Unit, bump_body);
    let actor = actor(&mut ids, "Ticker", vec![tick, bump]);

    let pipeline = lower_hir_module(&empty_module(vec![HirItem::Actor(actor)]));

    assert_eq!(pipeline.actor_layouts.len(), 1);
    let layout = &pipeline.actor_layouts[0];
    let tick_layout = layout
        .handlers
        .iter()
        .find(|h| h.name == "tick")
        .expect("tick handler layout");
    assert_eq!(
        tick_layout.every_ms,
        Some(50),
        "every_ns=50_000_000 must lower to every_ms=50"
    );
    assert_ne!(
        tick_layout.msg_type,
        i32::MAX,
        "periodic handler must carry the descriptor msg id, not the sentinel"
    );
    let bump_layout = layout
        .handlers
        .iter()
        .find(|h| h.name == "bump")
        .expect("bump handler layout");
    assert_eq!(
        bump_layout.every_ms, None,
        "message-driven handler must not carry an interval"
    );
}

#[test]
fn actor_handler_generator_receive_fn_lowers_to_generator_body() {
    // A `receive gen fn` handler lowers through the shared GenBlock state-machine
    // path: the HIR lower wraps the handler body in a `HirExprKind::GenBlock`
    // tail typed `Generator<Yield = i64, Return = Unit>` (see
    // `lower_actor_generator_body`). MIR lowers that handler as a generator —
    // the handler shell emits `Terminator::MakeGenerator` and the yield body is
    // surfaced as a sibling `__hew_gen_body_*` function — with NO UnsupportedNode
    // diagnostic. Before the fix, the handler was skipped with a "separate lane"
    // diagnostic and produced no MIR body.
    let mut ids = IdGen::default();
    let yield_value = literal_expr(&mut ids, HirLiteral::Integer(7), ResolvedTy::I64);
    let yield_expr = HirExpr {
        node: ids.node(),
        site: ids.site(),
        ty: ResolvedTy::Unit,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::Yield {
            value: Some(Box::new(yield_value)),
            yield_ty: ResolvedTy::I64,
        },
        span: 0..0,
    };
    let yield_stmt = HirStmt {
        node: ids.node(),
        kind: HirStmtKind::Expr(yield_expr),
        span: 0..0,
    };
    let gen_inner = block(&mut ids, vec![yield_stmt], None, ResolvedTy::Unit);
    let generator_ty = ResolvedTy::Named {
        name: "Generator".to_string(),
        args: vec![ResolvedTy::I64, ResolvedTy::Unit],
        builtin: Some(BuiltinType::Generator),
        is_opaque: false,
    };
    let gen_block = HirExpr {
        node: ids.node(),
        site: ids.site(),
        ty: generator_ty.clone(),
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::GenBlock {
            body: gen_inner,
            yield_ty: ResolvedTy::I64,
            return_ty: ResolvedTy::Unit,
            captures: Vec::new(),
        },
        span: 0..0,
    };
    let generator_body = block(&mut ids, vec![], Some(gen_block), generator_ty);
    let ticks = receive("ticks", true, vec![], ResolvedTy::I64, generator_body);
    let actor = actor(&mut ids, "Counter", vec![ticks]);

    let pipeline = lower_hir_module(&empty_module(vec![HirItem::Actor(actor)]));

    // No "separate lane" UnsupportedNode diagnostic for the generator handler.
    assert!(!pipeline.diagnostics.iter().any(|diag| {
        matches!(
            &diag.kind,
            MirDiagnosticKind::UnsupportedNode { reason }
                if reason == "actor receive fn declared as generator; generator MIR lowering is a separate lane"
        )
    }));
    // The handler shell exists and materialises the generator handle.
    let handler = pipeline
        .raw_mir
        .iter()
        .find(|func| func.name == "Counter__recv__ticks")
        .expect("generator handler must produce a MIR function");
    assert!(
        handler
            .blocks
            .iter()
            .any(|b| matches!(b.terminator, Terminator::MakeGenerator { .. })),
        "generator handler must emit Terminator::MakeGenerator"
    );
    // The yield body is surfaced as a sibling gen-body function with a Yield.
    let gen_body = pipeline
        .raw_mir
        .iter()
        .find(|func| {
            func.name
                .starts_with("__hew_gen_body_Counter__recv__ticks_")
        })
        .expect("generator handler must surface a __hew_gen_body_* function");
    assert!(
        gen_body
            .blocks
            .iter()
            .any(|b| matches!(b.terminator, Terminator::Yield { .. })),
        "generator body must contain a Terminator::Yield"
    );
}

#[test]
fn actor_lifecycle_start_lowers_and_unwired_hooks_fail_closed() {
    let mut ids = IdGen::default();
    let start_return = return_none_stmt(&mut ids);
    let crash_return = return_none_stmt(&mut ids);
    let upgrade_return = return_none_stmt(&mut ids);
    let actor = {
        let mut actor = actor(&mut ids, "Counter", vec![]);
        actor.lifecycle_hooks = vec![
            HirLifecycleHook {
                kind: HirLifecycleHookKind::Start,
                name: "boot".to_string(),
                params: vec![],
                return_ty: ResolvedTy::Unit,
                body: block(&mut ids, vec![start_return], None, ResolvedTy::Unit),
                span: 0..0,
            },
            HirLifecycleHook {
                kind: HirLifecycleHookKind::Crash,
                name: "crashed".to_string(),
                params: vec![],
                return_ty: ResolvedTy::Unit,
                body: block(&mut ids, vec![crash_return], None, ResolvedTy::Unit),
                span: 0..0,
            },
            HirLifecycleHook {
                kind: HirLifecycleHookKind::Upgrade,
                name: "upgrade".to_string(),
                params: vec![],
                return_ty: ResolvedTy::Unit,
                body: block(&mut ids, vec![upgrade_return], None, ResolvedTy::Unit),
                span: 0..0,
            },
        ];
        actor
    };

    let pipeline = lower_hir_module(&empty_module(vec![HirItem::Actor(actor)]));

    assert_eq!(
        pipeline.actor_layouts[0].on_start_symbol.as_deref(),
        Some("Counter__on_start")
    );
    let start = pipeline
        .raw_mir
        .iter()
        .find(|func| func.name == "Counter__on_start")
        .expect("start hook MIR");
    assert_eq!(start.call_conv, FunctionCallConv::ActorHandler);
    // on(crash) is now wired — it must lower to a MIR function.
    assert_eq!(
        pipeline.actor_layouts[0].on_crash_symbol.as_deref(),
        Some("Counter__on_crash"),
        "on(crash) hook must surface on_crash_symbol in ActorLayout"
    );
    let crash_fn = pipeline
        .raw_mir
        .iter()
        .find(|func| func.name == "Counter__on_crash")
        .expect("on(crash) hook must produce a MIR function");
    assert_eq!(crash_fn.call_conv, FunctionCallConv::ActorHandler);
    // on(upgrade) remains fail-closed — no MIR function, a diagnostic.
    assert!(
        pipeline.diagnostics.iter().any(|diag| {
            matches!(
                &diag.kind,
                MirDiagnosticKind::UnsupportedNode { reason } if reason.contains("OnUpgradeNotYetWired")
            )
        }),
        "OnUpgradeNotYetWired must still be fail-closed"
    );
    // on(stop) is wired — no fail-closed diagnostic for it.
    assert!(
        !pipeline.diagnostics.iter().any(|diag| {
            matches!(
                &diag.kind,
                MirDiagnosticKind::UnsupportedNode { reason } if reason.contains("OnStop")
            )
        }),
        "on(stop) must not emit a fail-closed diagnostic after wiring"
    );
    // on(crash) is wired — no fail-closed diagnostic for it.
    assert!(
        !pipeline.diagnostics.iter().any(|diag| {
            matches!(
                &diag.kind,
                MirDiagnosticKind::UnsupportedNode { reason } if reason.contains("OnCrashNotYetWired")
            )
        }),
        "on(crash) must not emit a fail-closed diagnostic after wiring"
    );
}

#[test]
fn actor_lifecycle_stop_lowers_to_actor_handler_function() {
    let mut ids = IdGen::default();
    let stop_return = return_none_stmt(&mut ids);
    let actor = {
        let mut actor = actor(&mut ids, "Counter", vec![]);
        actor.lifecycle_hooks = vec![HirLifecycleHook {
            kind: HirLifecycleHookKind::Stop,
            name: "shutdown".to_string(),
            params: vec![],
            return_ty: ResolvedTy::Unit,
            body: block(&mut ids, vec![stop_return], None, ResolvedTy::Unit),
            span: 0..0,
        }];
        actor
    };

    let pipeline = lower_hir_module(&empty_module(vec![HirItem::Actor(actor)]));

    // on_stop_symbols populated in the actor layout (one hook → one entry at index 0).
    assert_eq!(
        pipeline.actor_layouts[0].on_stop_symbols.as_slice(),
        &["Counter__on_stop__0"]
    );
    // MIR function emitted with ActorHandler calling convention.
    let stop = pipeline
        .raw_mir
        .iter()
        .find(|func| func.name == "Counter__on_stop__0")
        .expect("on(stop) hook must produce a MIR function");
    assert_eq!(stop.call_conv, FunctionCallConv::ActorHandler);
    // No fail-closed diagnostic emitted.
    assert!(
        pipeline.diagnostics.is_empty(),
        "on(stop) lowering must not emit diagnostics; got: {:?}",
        pipeline.diagnostics
    );
}

#[test]
fn actor_multiple_stop_hooks_lower_to_distinct_indexed_symbols_without_collision() {
    let mut ids = IdGen::default();
    let stop_return_a = return_none_stmt(&mut ids);
    let stop_return_b = return_none_stmt(&mut ids);
    let actor = {
        let mut actor = actor(&mut ids, "Counter", vec![]);
        actor.lifecycle_hooks = vec![
            HirLifecycleHook {
                kind: HirLifecycleHookKind::Stop,
                name: "cleanup_a".to_string(),
                params: vec![],
                return_ty: ResolvedTy::Unit,
                body: block(&mut ids, vec![stop_return_a], None, ResolvedTy::Unit),
                span: 0..0,
            },
            HirLifecycleHook {
                kind: HirLifecycleHookKind::Stop,
                name: "cleanup_b".to_string(),
                params: vec![],
                return_ty: ResolvedTy::Unit,
                body: block(&mut ids, vec![stop_return_b], None, ResolvedTy::Unit),
                span: 0..0,
            },
        ];
        actor
    };

    let pipeline = lower_hir_module(&empty_module(vec![HirItem::Actor(actor)]));

    // Both stop hooks must yield distinct indexed symbols in declaration order.
    assert_eq!(
        pipeline.actor_layouts[0].on_stop_symbols.as_slice(),
        &["Counter__on_stop__0", "Counter__on_stop__1"],
        "two on(stop) hooks must produce two indexed symbols in lexical order"
    );
    // Both MIR functions must be emitted with ActorHandler calling convention.
    for sym in &["Counter__on_stop__0", "Counter__on_stop__1"] {
        let func = pipeline
            .raw_mir
            .iter()
            .find(|f| &f.name == sym)
            .unwrap_or_else(|| panic!("on(stop) hook `{sym}` must produce a MIR function"));
        assert_eq!(func.call_conv, FunctionCallConv::ActorHandler);
    }
    // No diagnostics — specifically no ActorHandlerSymbolCollision.
    assert!(
        pipeline.diagnostics.is_empty(),
        "multiple on(stop) hooks must not emit diagnostics; got: {:?}",
        pipeline.diagnostics
    );
}

#[test]
fn actor_handler_unknown_self_field_emits_typed_diagnostic_and_skips_handler() {
    let mut ids = IdGen::default();
    let condition = literal_expr(&mut ids, HirLiteral::Bool(true), ResolvedTy::Bool);
    let then_expr = self_field_expr(&mut ids, "Counter", "missing");
    let body_expr = HirExpr {
        node: ids.node(),
        site: ids.site(),
        ty: ResolvedTy::I64,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::If {
            condition: Box::new(condition),
            then_expr: Box::new(then_expr),
            else_expr: None,
        },
        span: 0..0,
    };
    let bad_body = block(&mut ids, vec![], Some(body_expr), ResolvedTy::I64);
    let bad = receive("bad", false, vec![], ResolvedTy::I64, bad_body);
    let actor = actor(&mut ids, "Counter", vec![bad]);

    let pipeline = lower_hir_module(&empty_module(vec![HirItem::Actor(actor)]));

    assert!(pipeline
        .raw_mir
        .iter()
        .all(|func| func.name != "Counter__recv__bad"));
    assert!(pipeline.diagnostics.iter().any(|diag| {
        matches!(
            &diag.kind,
            MirDiagnosticKind::UnknownActorStateField { actor, field }
                if actor == "Counter" && field == "missing"
        )
    }));
}

#[test]
fn actor_handler_symbol_collision_emits_typed_diagnostic_and_skips_handler() {
    let mut ids = IdGen::default();
    let top_level_return = return_none_stmt(&mut ids);
    let top_level_body = block(&mut ids, vec![top_level_return], None, ResolvedTy::Unit);
    let top_level = HirFn {
        id: ids.item(),
        node: ids.node(),
        name: "Counter__recv__ping".to_string(),
        type_params: vec![],
        params: vec![],
        return_ty: ResolvedTy::Unit,
        body: top_level_body,
        span: 0..0,
        is_generator: false,
        intrinsic_id: None,
    };
    let ping_return = return_none_stmt(&mut ids);
    let ping_body = block(&mut ids, vec![ping_return], None, ResolvedTy::Unit);
    let ping = receive("ping", false, vec![], ResolvedTy::Unit, ping_body);
    let actor = actor(&mut ids, "Counter", vec![ping]);

    let pipeline = lower_hir_module(&empty_module(vec![
        HirItem::Function(top_level),
        HirItem::Actor(actor),
    ]));

    assert_eq!(
        pipeline
            .raw_mir
            .iter()
            .filter(|func| func.name == "Counter__recv__ping")
            .count(),
        1,
        "colliding actor handler must not emit a second MIR function"
    );
    assert!(pipeline.diagnostics.iter().any(|diag| {
        matches!(
            &diag.kind,
            MirDiagnosticKind::ActorHandlerSymbolCollision { symbol, .. }
                if symbol == "Counter__recv__ping"
        )
    }));
}

#[test]
fn actor_lifecycle_crash_lowers_to_actor_handler_function() {
    let mut ids = IdGen::default();
    let crash_return = return_none_stmt(&mut ids);
    let actor = {
        let mut actor = actor(&mut ids, "Worker", vec![]);
        actor.lifecycle_hooks = vec![HirLifecycleHook {
            kind: HirLifecycleHookKind::Crash,
            name: "handle_crash".to_string(),
            params: vec![],
            return_ty: ResolvedTy::Unit,
            body: block(&mut ids, vec![crash_return], None, ResolvedTy::Unit),
            span: 0..0,
        }];
        actor
    };

    let pipeline = lower_hir_module(&empty_module(vec![HirItem::Actor(actor)]));

    // on_crash_symbol populated in the actor layout.
    assert_eq!(
        pipeline.actor_layouts[0].on_crash_symbol.as_deref(),
        Some("Worker__on_crash"),
        "on(crash) hook must surface as on_crash_symbol in ActorLayout"
    );
    // MIR function emitted with ActorHandler calling convention.
    let crash_fn = pipeline
        .raw_mir
        .iter()
        .find(|func| func.name == "Worker__on_crash")
        .expect("on(crash) hook must produce a MIR function");
    assert_eq!(crash_fn.call_conv, FunctionCallConv::ActorHandler);
    // No fail-closed diagnostic emitted.
    assert!(
        pipeline.diagnostics.is_empty(),
        "on(crash) lowering must not emit diagnostics; got: {:?}",
        pipeline.diagnostics
    );
}
