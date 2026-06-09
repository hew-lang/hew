use std::collections::HashMap;

use hew_hir::{
    ids::IdGen, HirActorDecl, HirActorReceiveFn, HirActorStateGuard, HirBinding, HirBlock, HirExpr,
    HirExprKind, HirField, HirFn, HirItem, HirLifecycleHook, HirLifecycleHookKind, HirLiteral,
    HirModule, HirStmt, HirStmtKind, IntentKind, ResolvedRef, ScopeId, ValueClass,
};
use hew_mir::{lower_hir_module, FunctionCallConv, Instr, MirDiagnosticKind, Terminator};
use hew_types::ResolvedTy;

fn empty_module(items: Vec<HirItem>) -> HirModule {
    HirModule {
        items,
        type_classes: HashMap::default(),
        monomorphisations: vec![],
        call_site_type_args: HashMap::default(),
        record_layouts: vec![],
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

fn self_field_expr(ids: &mut IdGen, actor_name: &str, field: &str) -> HirExpr {
    let self_expr = HirExpr {
        node: ids.node(),
        site: ids.site(),
        ty: ResolvedTy::Named {
            name: actor_name.to_string(),
            args: vec![],
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
    HirActorDecl {
        id: ids.item(),
        node: ids.node(),
        name: name.to_string(),
        state_fields: vec![HirField {
            name: "count".to_string(),
            ty: ResolvedTy::I64,
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
        span: 0..0,
    }
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

#[test]
fn actor_handler_generator_receive_fn_emits_unsupported_diagnostic_and_no_body() {
    let mut ids = IdGen::default();
    let generator_return = return_none_stmt(&mut ids);
    let generator_body = block(&mut ids, vec![generator_return], None, ResolvedTy::Unit);
    let ticks = receive("ticks", true, vec![], ResolvedTy::I64, generator_body);
    let actor = actor(&mut ids, "Counter", vec![ticks]);

    let pipeline = lower_hir_module(&empty_module(vec![HirItem::Actor(actor)]));

    assert!(pipeline
        .raw_mir
        .iter()
        .all(|func| func.name != "Counter__recv__ticks"));
    assert!(pipeline.diagnostics.iter().any(|diag| {
        matches!(
            &diag.kind,
            MirDiagnosticKind::UnsupportedNode { reason }
                if reason == "actor receive fn declared as generator; generator MIR lowering is a separate lane"
        )
    }));
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
    // on(crash) and on(upgrade) remain fail-closed until their runtime wiring lands.
    for diagnostic_name in ["OnCrashNotYetWired", "OnUpgradeNotYetWired"] {
        assert!(
            pipeline.diagnostics.iter().any(|diag| {
                matches!(
                    &diag.kind,
                    MirDiagnosticKind::UnsupportedNode { reason } if reason.contains(diagnostic_name)
                )
            }),
            "{diagnostic_name} must still be fail-closed"
        );
    }
    // on(stop) is now wired — no fail-closed diagnostic for it.
    assert!(
        !pipeline.diagnostics.iter().any(|diag| {
            matches!(
                &diag.kind,
                MirDiagnosticKind::UnsupportedNode { reason } if reason.contains("OnStop")
            )
        }),
        "on(stop) must not emit a fail-closed diagnostic after wiring"
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

    // on_stop_symbol populated in the actor layout.
    assert_eq!(
        pipeline.actor_layouts[0].on_stop_symbol.as_deref(),
        Some("Counter__on_stop")
    );
    // MIR function emitted with ActorHandler calling convention.
    let stop = pipeline
        .raw_mir
        .iter()
        .find(|func| func.name == "Counter__on_stop")
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
