//! Acceptance tests for the MIR actor-handler state-guard layout contract.
//!
//! Every non-generator receive handler must carry an
//! `ActorHandlerLayout::requires_state_guard` fact derived from the
//! checker-owned `HirActorStateGuard` on the HIR node.
//!
//! The state-guard contract is carried as layout metadata, not as emitted
//! acquire/release instructions in the handler body.  The acquire and release
//! calls currently fire from the runtime scheduler
//! (`hew-runtime/src/scheduler.rs:810,870,959`); the metadata field exists
//! so MIR analysis passes and future generated-code bracketing can read the
//! contract without re-deriving it from HIR.
//!
//! These tests verify HIR→MIR propagation of the checker-owned fact, not
//! re-derivation; the checker is the single fact-producer.

use std::collections::HashMap;

use hew_hir::{
    ids::IdGen, HirActorDecl, HirActorReceiveFn, HirActorStateGuard, HirBinding, HirBlock,
    HirField, HirItem, HirModule, HirStmt, HirStmtKind, ScopeId,
};
use hew_mir::{lower_hir_module, MirDiagnosticKind};
use hew_types::{ActorHandlerSpec, ActorProtocolDescriptor, ResolvedTy};

// --- helpers ----------------------------------------------------------------

fn empty_module(items: Vec<HirItem>) -> HirModule {
    HirModule {
        items,
        diagnostic_source_modules: HashMap::default(),
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

fn ids() -> IdGen {
    IdGen::default()
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

fn empty_unit_block(ids: &mut IdGen) -> HirBlock {
    let stmt = HirStmt {
        node: ids.node(),
        kind: HirStmtKind::Return(None),
        span: 0..0,
    };
    HirBlock {
        node: ids.node(),
        scope: ScopeId(0),
        statements: vec![stmt],
        tail: None,
        ty: ResolvedTy::Unit,
        span: 0..0,
    }
}

fn receive_fn(
    ids: &mut IdGen,
    name: &str,
    state_guard: HirActorStateGuard,
    is_generator: bool,
    params: Vec<HirBinding>,
) -> HirActorReceiveFn {
    HirActorReceiveFn {
        name: name.to_string(),
        is_generator,
        params,
        return_ty: ResolvedTy::Unit,
        body: empty_unit_block(ids),
        state_guard,
        every_ns: None,
        span: 0..0,
    }
}

fn minimal_actor(
    ids: &mut IdGen,
    name: &str,
    receive_handlers: Vec<HirActorReceiveFn>,
) -> HirActorDecl {
    let specs: Vec<ActorHandlerSpec> = receive_handlers
        .iter()
        .map(|h| ActorHandlerSpec {
            name: h.name.clone(),
            param_tys: h.params.iter().map(|p| p.ty.clone()).collect(),
            return_ty: h.return_ty.clone(),
            symbol: format!("{name}__{}", h.name),
        })
        .collect();
    let protocol_descriptor = if specs.is_empty() {
        None
    } else {
        Some(
            ActorProtocolDescriptor::from_handlers(name.to_string(), &specs)
                .expect("test actor handler names must not collide"),
        )
    };
    let init_body = empty_unit_block(ids);
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
            params: vec![binding(ids, "initial", ResolvedTy::I64)],
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

// --- tests ------------------------------------------------------------------

/// Every non-generator receive handler lowered from `HirActorStateGuard::Exclusive`
/// must produce an `ActorHandlerLayout` with `requires_state_guard = true`.
///
/// This pins the HIR→MIR propagation path that MIR analysis passes and
/// codegen consume. The checker is the only authority for the guard fact;
/// MIR must not re-derive it.
#[test]
fn actor_handler_layout_records_exclusive_state_guard_from_hir() {
    let mut ids = ids();
    let handler = receive_fn(
        &mut ids,
        "inc",
        HirActorStateGuard::Exclusive,
        false,
        vec![],
    );
    let actor = minimal_actor(&mut ids, "Counter", vec![handler]);

    let pipeline = lower_hir_module(&empty_module(vec![HirItem::Actor(actor)]));

    assert!(
        pipeline.diagnostics.is_empty(),
        "no diagnostics expected on a well-formed actor: {:?}",
        pipeline.diagnostics
    );
    assert_eq!(pipeline.actor_layouts.len(), 1, "one actor layout expected");
    let layout = &pipeline.actor_layouts[0];
    assert_eq!(layout.handlers.len(), 1, "one handler expected");
    assert!(
        layout.handlers[0].requires_state_guard,
        "HirActorStateGuard::Exclusive must map to requires_state_guard=true in the layout"
    );
}

/// All non-generator receive handlers on an actor propagate their guard fact
/// to the layout, regardless of the number of handlers or their parameter lists.
///
/// This is the breadth test: multiple handlers, all exclusive, all appear in
/// the layout with `requires_state_guard=true`.
#[test]
fn non_generator_receive_handler_propagates_guard_to_layout() {
    let mut ids = ids();
    let amount = binding(&mut ids, "amount", ResolvedTy::I64);
    let handlers = vec![
        receive_fn(
            &mut ids,
            "increment",
            HirActorStateGuard::Exclusive,
            false,
            vec![amount],
        ),
        receive_fn(
            &mut ids,
            "reset",
            HirActorStateGuard::Exclusive,
            false,
            vec![],
        ),
        receive_fn(
            &mut ids,
            "current",
            HirActorStateGuard::Exclusive,
            false,
            vec![],
        ),
    ];
    let actor = minimal_actor(&mut ids, "Counter", handlers);

    let pipeline = lower_hir_module(&empty_module(vec![HirItem::Actor(actor)]));

    assert!(
        pipeline.diagnostics.is_empty(),
        "no diagnostics expected: {:?}",
        pipeline.diagnostics
    );
    let layout = &pipeline.actor_layouts[0];
    assert_eq!(
        layout.handlers.len(),
        3,
        "all three non-generator handlers must appear in the layout"
    );
    for handler in &layout.handlers {
        assert!(
            handler.requires_state_guard,
            "handler `{}` must carry requires_state_guard=true",
            handler.name
        );
    }
}

/// Generator receive handlers (`receive gen fn`) are stream producers, not
/// request/reply message handlers, so they must NOT appear in
/// `actor_layouts.handlers` (the message-dispatch layout). Their MIR *body*
/// now lowers (through the `GenBlock` state-machine path), but the
/// guard-propagation loop in `lower_actor_handler_layouts` still skips them:
/// the consumer-side stream-dispatch bridge that would route a message to a
/// generator handler is a separate, not-yet-built piece.
#[test]
fn generator_handler_is_absent_from_actor_handler_layout() {
    let mut ids = ids();
    // One normal handler + one generator handler.
    let normal = receive_fn(
        &mut ids,
        "inc",
        HirActorStateGuard::Exclusive,
        false,
        vec![],
    );
    let generator = receive_fn(
        &mut ids,
        "ticks",
        HirActorStateGuard::Exclusive,
        true, // is_generator
        vec![],
    );
    // Protocol descriptor only for the normal handler — generator is excluded
    // from the protocol (no msg_type).
    let specs = vec![ActorHandlerSpec {
        name: "inc".to_string(),
        param_tys: vec![],
        return_ty: ResolvedTy::Unit,
        symbol: "Counter__inc".to_string(),
    }];
    let descriptor = ActorProtocolDescriptor::from_handlers("Counter".to_string(), &specs)
        .expect("non-colliding handler list");
    let actor = HirActorDecl {
        id: ids.item(),
        node: ids.node(),
        name: "Counter".to_string(),
        defining_module: None,
        state_fields: vec![],
        init: None,
        receive_handlers: vec![normal, generator],
        methods: vec![],
        lifecycle_hooks: vec![],
        max_heap_bytes: None,
        is_isolated: false,
        mailbox_capacity: None,
        overflow_policy: None,
        cycle_capable: false,
        protocol_descriptor: Some(descriptor),
        span: 0..0,
    };

    let pipeline = lower_hir_module(&empty_module(vec![HirItem::Actor(actor)]));

    // No "separate lane" UnsupportedNode diagnostic — the generator handler now
    // lowers rather than bailing out.
    assert!(
        !pipeline.diagnostics.iter().any(|d| {
            matches!(
                &d.kind,
                MirDiagnosticKind::UnsupportedNode { reason }
                    if reason == "actor receive fn declared as generator; generator MIR lowering is a separate lane"
            )
        }),
        "generator handler must no longer emit the separate-lane diagnostic; got: {:?}",
        pipeline.diagnostics
    );

    // The message-dispatch layout must contain only the non-generator handler,
    // with the guard fact set — the generator handler stays out of the layout.
    let layout = &pipeline.actor_layouts[0];
    assert_eq!(
        layout.handlers.len(),
        1,
        "generator handler must not appear in the handler layout"
    );
    assert_eq!(layout.handlers[0].name, "inc");
    assert!(
        layout.handlers[0].requires_state_guard,
        "the non-generator handler must still carry requires_state_guard=true"
    );
}
