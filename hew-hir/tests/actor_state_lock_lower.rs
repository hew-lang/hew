use hew_hir::{
    dump_hir, lower_program, HirActorStateGuard, HirDiagnosticKind, HirItem, ResolutionCtx,
};
use hew_types::{module_registry::ModuleRegistry, Checker, TypeCheckOutput};

fn lower_checked(source: &str) -> hew_hir::LowerOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    lower_program(&parsed.program, &tc_output, &ResolutionCtx)
}

#[test]
fn hir_receive_handler_carries_state_guard() {
    let output = lower_checked(
        r"
        actor Counter {
            let count: i32;
            receive fn inc(n: i32) {
                let seen: i32 = n;
            }
        }
        fn main() {}
        ",
    );

    assert!(
        output.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        output.diagnostics
    );
    let actor = output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Actor(actor) if actor.name == "Counter" => Some(actor),
            _ => None,
        })
        .expect("Counter actor should lower");
    assert_eq!(
        actor.receive_handlers[0].state_guard,
        HirActorStateGuard::Exclusive
    );
}

#[test]
fn dump_includes_state_guard_fact() {
    let output = lower_checked(
        r"
        actor Counter {
            receive fn inc() {}
        }
        fn main() {}
        ",
    );

    let dump = dump_hir(&output.module);
    assert!(
        dump.contains("receive inc params=0 -> () state_guard=Exclusive"),
        "HIR dump must expose state guard fact, got:\n{dump}"
    );
}

#[test]
fn missing_guard_fact_fails_closed() {
    let parsed = hew_parser::parse(
        r"
        actor Counter {
            receive fn inc() {}
        }
        fn main() {}
        ",
    );
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );

    let output = lower_program(&parsed.program, &TypeCheckOutput::default(), &ResolutionCtx);
    assert!(
        output.diagnostics.iter().any(|diagnostic| matches!(
            diagnostic.kind,
            HirDiagnosticKind::ActorStateGuardMissing { .. }
        )),
        "missing checker guard fact must be surfaced as a diagnostic: {:?}",
        output.diagnostics
    );
}
