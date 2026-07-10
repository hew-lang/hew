use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::model::CoalesceKeyKind;
use hew_mir::{lower_hir_module, MirDiagnosticKind};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn lower_source(source: &str) -> hew_mir::IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    let hir = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        hir.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        hir.diagnostics
    );
    lower_hir_module(&hir.module)
}

#[test]
fn resolves_supported_key_entries_and_fallback() {
    let pipeline = lower_source(
        r"
        actor Prices {
            mailbox 4 overflow coalesce(symbol) fallback drop_old;
            receive fn update(symbol: string, price: i64) {}
            receive fn report() -> i64 { 0 }
        }
        ",
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "unexpected diagnostics: {:#?}",
        pipeline.diagnostics
    );
    let actor = pipeline
        .actor_layouts
        .iter()
        .find(|layout| layout.name == "Prices")
        .expect("Prices layout");
    let plan = actor.coalesce_key_plan.as_ref().expect("coalesce plan");
    assert_eq!(plan.fallback, 2);
    assert_eq!(plan.entries.len(), 1);
    assert_eq!(plan.entries[0].param_index, 0);
    assert_eq!(plan.entries[0].kind, CoalesceKeyKind::StringHash);
    assert_eq!(plan.entries[0].msg_type, actor.handlers[0].msg_type);
}

#[test]
fn rejects_missing_or_unsupported_key_fields() {
    for (source, expected_reason) in [
        (
            r"
            actor Missing {
                mailbox 4 overflow coalesce(id);
                receive fn update(value: i64) {}
            }
            ",
            "no receive handler",
        ),
        (
            r"
            actor FloatKey {
                mailbox 4 overflow coalesce(id);
                receive fn update(id: f64) {}
            }
            ",
            "unsupported type",
        ),
    ] {
        let pipeline = lower_source(source);
        assert!(pipeline.diagnostics.iter().any(|diagnostic| {
            matches!(
                &diagnostic.kind,
                MirDiagnosticKind::MailboxOverflowCoalesceKeyFieldInvalid { reason, .. }
                    if reason.contains(expected_reason)
            )
        }));
    }
}

#[test]
fn q244_shim_allows_handlers_without_the_key_field() {
    let pipeline = lower_source(
        r"
        actor Mixed {
            mailbox 4 overflow coalesce(id);
            receive fn update(id: i64, value: i64) {}
            receive fn report() -> i64 { 0 }
        }
        ",
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "unexpected diagnostics: {:#?}",
        pipeline.diagnostics
    );
    let actor = pipeline
        .actor_layouts
        .iter()
        .find(|layout| layout.name == "Mixed")
        .expect("Mixed layout");
    assert_eq!(
        actor
            .coalesce_key_plan
            .as_ref()
            .expect("coalesce plan")
            .entries
            .len(),
        1
    );
}
