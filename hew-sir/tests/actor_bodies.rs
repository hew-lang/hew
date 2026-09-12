//! Actor methods, lifecycle hooks and generator receives are private actor
//! bodies entered with the exclusive state seat. A malformed body shape is
//! refused by the verifier, never realized by a later stage.

use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_sir::{
    verify_module, BoundaryDecision, SemCallableKind, SemModule, SemParamPassing, SemTerminator,
    SirDiagnosticKind, SirLoweringStatus,
};
use hew_types::{module_registry::ModuleRegistry, Checker};

const COUNTER: &str = r#"
actor Counter {
    var count: i64 = 0,
    var label: string = "count",
    fn describe(prefix: string) -> string { prefix + label }
    fn bump(by: i64) { count = count + by; }
    receive fn increment(by: i64) { bump(by); println(describe(label)); }
    #[on(start)]
    fn started() { count = 1; }
    #[on(stop)]
    fn stopping() { println(label); }
}
fn main() {
    let counter = spawn Counter();
    let _ = counter.increment(2);
    close(counter);
}
"#;

fn lower_source(source: &str) -> SemModule {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let mut checker = Checker::new(ModuleRegistry::new(Vec::new()));
    let facts = checker.check_program(&parsed.program);
    assert!(facts.errors.is_empty(), "{:?}", facts.errors);
    let hir = lower_program_host_target(&parsed.program, &facts, &ResolutionCtx);
    assert!(hir.diagnostics.is_empty(), "{:?}", hir.diagnostics);
    let lowered = hew_sir::lower_module(&hir.module, &facts);
    for status in &lowered.statuses {
        assert!(
            !matches!(status.status, SirLoweringStatus::Unsupported { .. }),
            "{}: {:?}",
            status.name,
            status.status
        );
    }
    let errors = verify_module(&lowered.module);
    assert!(errors.is_empty(), "{errors:#?}");
    lowered.module
}

fn body_named(module: &SemModule, suffix: &str) -> hew_sir::CallableId {
    module
        .callables
        .iter()
        .find(|callable| callable.symbol.ends_with(suffix))
        .unwrap_or_else(|| panic!("no callable ends with {suffix}"))
        .id
}

#[test]
fn methods_and_hooks_are_private_bodies_of_their_actor() {
    let module = lower_source(COUNTER);
    let actor = &module.actors[0];
    let method = body_named(&module, "method_describe");
    let start = body_named(&module, "hook_started");
    let stop = body_named(&module, "hook_stopping");
    assert!(actor.methods.contains(&method));
    assert_eq!(actor.start, Some(start));
    assert_eq!(actor.stop, vec![stop]);
    let describe = module.callable(method).unwrap();
    assert_eq!(describe.kind, SemCallableKind::HewActor(actor.id));
    assert_eq!(
        describe.signature.params[0].passing,
        SemParamPassing::BorrowMut
    );
    // The method lends its owned argument like an ordinary function.
    assert_eq!(
        describe.signature.params[1].passing,
        SemParamPassing::Borrow
    );
    let handler = module
        .functions
        .iter()
        .find(|function| function.callable == actor.handlers[0].callable)
        .unwrap();
    let seats: Vec<_> = handler
        .blocks
        .iter()
        .filter_map(|block| match &block.terminator {
            SemTerminator::Call { callee, args, .. } if actor.methods.contains(callee) => {
                Some(args[0].clone())
            }
            _ => None,
        })
        .collect();
    assert_eq!(seats.len(), 2, "both method calls lend the state seat");
    assert!(seats.iter().all(|seat| {
        seat.decision == BoundaryDecision::BorrowMut
            && seat.operand.value == handler.params[0].value
    }));
}

#[test]
fn a_body_returning_an_owned_state_field_copies_it_out_of_the_seat() {
    const SOURCE: &str = r#"
actor Ledger {
    var label: string = "ledger",
    fn describe() -> string { label }
    receive fn show() -> string { describe() }
    receive fn raw() -> string { label }
}
fn main() {
    let ledger = spawn Ledger();
    match ledger.show() {
        .Ok(label) => println(label),
        .Err(_) => panic("show failed"),
    }
    close(ledger);
}
"#;
    let module = lower_source(SOURCE);
    for suffix in ["method_describe", "Ledger__raw"] {
        let id = body_named(&module, suffix);
        let body = module
            .functions
            .iter()
            .find(|function| function.callable == id)
            .unwrap();
        let mut copies = 0;
        let mut takes = 0;
        for block in &body.blocks {
            for op in &block.ops {
                match op.kind {
                    hew_sir::SemOpKind::LoadCopy { .. } => copies += 1,
                    hew_sir::SemOpKind::LoadTake { .. } => takes += 1,
                    _ => {}
                }
            }
        }
        assert_eq!((copies, takes), (1, 0), "{suffix} must copy the field out");
    }
}

#[test]
fn verifier_refuses_a_method_call_that_consumes_the_state_seat() {
    let mut module = lower_source(COUNTER);
    let actor = module.actors[0].clone();
    let handler = module
        .functions
        .iter_mut()
        .find(|function| function.callable == actor.handlers[0].callable)
        .unwrap();
    let mut changed = false;
    for block in &mut handler.blocks {
        if let SemTerminator::Call { callee, args, .. } = &mut block.terminator {
            if actor.methods.contains(callee) {
                args[0].decision = BoundaryDecision::Move;
                changed = true;
                break;
            }
        }
    }
    assert!(changed);
    let diagnostics = verify_module(&module);
    assert!(
        diagnostics.iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            SirDiagnosticKind::InvalidOperation { reason, .. }
                if reason.contains("expected BorrowMut")
        )),
        "{diagnostics:#?}"
    );
}

#[test]
fn verifier_refuses_a_method_whose_seat_is_not_exclusive() {
    let mut module = lower_source(COUNTER);
    let method = body_named(&module, "method_bump");
    let callable = module
        .callables
        .iter_mut()
        .find(|callable| callable.id == method)
        .unwrap();
    callable.signature.params[0].passing = SemParamPassing::Consume;
    let diagnostics = verify_module(&module);
    assert!(
        diagnostics.iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            SirDiagnosticKind::InvalidTerminator { reason }
                if reason.contains("exclusive state ABI")
        )),
        "{diagnostics:#?}"
    );
}

#[test]
fn verifier_refuses_a_method_call_from_another_actor() {
    let mut module = lower_source(COUNTER);
    let actor = module.actors[0].clone();
    let method = body_named(&module, "method_bump");
    // Detach the method from its actor: the same call now names a body the
    // caller does not own.
    module.actors[0].methods.retain(|body| *body != method);
    module.actors[0]
        .handlers
        .retain(|handler| handler.callable != actor.handlers[0].callable);
    let diagnostics = verify_module(&module);
    assert!(
        diagnostics.iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            SirDiagnosticKind::InvalidOperation { reason, .. }
                if reason.contains("outside SIR's default HewDirect ABI domain")
        )),
        "{diagnostics:#?}"
    );
}

const STREAM: &str = r"
actor Source {
    var base: i64 = 10,
    receive gen fn items(count: i64) -> i64 {
        for index in 0..count {
            base = base + 1;
            yield base + index;
        }
    }
}
fn main() {
    let source = spawn Source();
    for item in source.items(3) { println(item); }
    close(source);
}
";

#[test]
fn stream_producers_yield_into_the_request_sink_and_snapshot_state() {
    let module = lower_source(STREAM);
    let actor = &module.actors[0];
    let handler = &actor.handlers[0];
    assert_eq!(handler.stream, Some(hew_types::ResolvedTy::I64));
    assert_eq!(handler.return_ty, hew_types::ResolvedTy::Unit);
    let sink = handler.params.last().unwrap();
    assert_eq!(
        hew_sir::sink_element(sink),
        Some(&hew_types::ResolvedTy::I64)
    );
    let producer = module
        .callable(handler.callable)
        .expect("producer callable");
    assert_eq!(
        producer.signature.params.last().unwrap().passing,
        SemParamPassing::Consume,
        "the sink is owned by the producer turn"
    );
    let body = module
        .functions
        .iter()
        .find(|function| function.callable == handler.callable)
        .unwrap();
    let sends: Vec<_> = body
        .blocks
        .iter()
        .filter_map(|block| match &block.terminator {
            SemTerminator::Suspend {
                kind: hew_sir::SuspendKind::StreamSend,
                inputs,
                resumes,
                ..
            } => Some((inputs.clone(), resumes.len())),
            _ => None,
        })
        .collect();
    assert_eq!(sends.len(), 1);
    let (inputs, resumes) = &sends[0];
    assert_eq!(
        *resumes, 2,
        "an accepted send and a closed consumer both resume"
    );
    assert_eq!(inputs[0].decision, BoundaryDecision::Borrow);
    assert_eq!(inputs[0].operand.value, body.params.last().unwrap().value);
    assert_eq!(inputs[1].decision, BoundaryDecision::Move);
    // The body writes `base` through a local snapshot, never the state seat.
    assert!(body
        .blocks
        .iter()
        .flat_map(|block| &block.ops)
        .all(|op| !matches!(
            op.kind,
            hew_sir::SemOpKind::StoreAssign { place, .. }
                if body.places.iter().any(|declaration| declaration.id == place
                    && matches!(declaration.origin, hew_sir::PlaceOrigin::ActorState { .. }))
        )));
    let main = module
        .functions
        .iter()
        .find(|function| Some(function.callable) == module.entry_callable)
        .unwrap();
    assert!(main
        .blocks
        .iter()
        .flat_map(|block| &block.ops)
        .any(|op| matches!(op.kind, hew_sir::SemOpKind::StreamPipe { .. })));
    assert!(main.blocks.iter().any(|block| matches!(
        block.terminator,
        SemTerminator::ActorCall {
            operation: hew_sir::ActorOperation::StreamStart { .. },
            ..
        }
    )));
    assert!(main.blocks.iter().any(|block| matches!(
        block.terminator,
        SemTerminator::Suspend {
            kind: hew_sir::SuspendKind::StreamNext { park: true },
            ..
        }
    )));
}

#[test]
fn verifier_refuses_a_stream_send_that_keeps_its_element() {
    let mut module = lower_source(STREAM);
    let handler = module.actors[0].handlers[0].callable;
    let body = module
        .functions
        .iter_mut()
        .find(|function| function.callable == handler)
        .unwrap();
    let mut changed = false;
    for block in &mut body.blocks {
        if let SemTerminator::Suspend {
            kind: hew_sir::SuspendKind::StreamSend,
            inputs,
            ..
        } = &mut block.terminator
        {
            inputs[1].decision = BoundaryDecision::Borrow;
            changed = true;
        }
    }
    assert!(changed);
    let diagnostics = verify_module(&module);
    assert!(
        diagnostics.iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            SirDiagnosticKind::InvalidTerminator { reason }
                if reason.contains("StreamSend suspension")
        )),
        "{diagnostics:#?}"
    );
}

#[test]
fn verifier_refuses_a_notification_hook_with_a_scalar_payload() {
    for exit_hook in [true, false] {
        let mut module = lower_source(COUNTER);
        let actor = &mut module.actors[0];
        let hook = actor.stop.pop().expect("counter stop hook");
        if exit_hook {
            actor.exit = Some(hook);
        } else {
            actor.down = Some(hook);
        }
        let callable = module
            .callables
            .iter_mut()
            .find(|callable| callable.id == hook)
            .unwrap();
        callable.signature.params.push(hew_sir::SemAbiParam {
            ty: hew_types::ResolvedTy::I64,
            passing: SemParamPassing::ReadOnly,
            caller_visible_projection: false,
        });
        let diagnostics = verify_module(&module);
        assert!(
            diagnostics.iter().any(|diagnostic| matches!(
                &diagnostic.kind,
                SirDiagnosticKind::InvalidTerminator { reason }
                    if reason.contains("lifecycle hook signature differs")
            )),
            "{diagnostics:#?}"
        );
    }
}
