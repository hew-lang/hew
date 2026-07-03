use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{lower_hir_module, Instr, MirDiagnosticKind, Place};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn lower_module_from_source(source: &str) -> hew_mir::IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "type errors: {:?}",
        tc_output.errors
    );
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
fn all_defaulted_actor_state_fields_allow_no_arg_spawn_and_use_defaults() {
    let pipeline = lower_module_from_source(
        r"
        actor Counter {
            var n: i64 = 7;
            receive fn ping() {}
        }

        fn main() -> i64 {
            spawn Counter;
            0
        }
        ",
    );

    assert!(
        pipeline.diagnostics.is_empty(),
        "unexpected MIR diagnostics: {:#?}",
        pipeline.diagnostics
    );
    let main_fn = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "main")
        .expect("main emitted");
    let state_field_src = main_fn
        .blocks
        .iter()
        .flat_map(|b| b.instructions.iter())
        .find_map(|instr| match instr {
            Instr::RecordInit { fields, .. } => fields.first().map(|(_, src)| *src),
            _ => None,
        })
        .expect("Counter state RecordInit field source");
    let default_const = main_fn
        .blocks
        .iter()
        .flat_map(|b| b.instructions.iter())
        .find_map(|instr| match instr {
            Instr::ConstI64 { dest, value } if *dest == state_field_src => Some(*value),
            _ => None,
        });
    assert_eq!(
        default_const,
        Some(7),
        "defaulted state field must use the declared default, not zero"
    );
    assert!(
        main_fn
            .blocks
            .iter()
            .flat_map(|b| b.instructions.iter())
            .any(|instr| matches!(instr, Instr::SpawnActor { actor_name, state: Some(Place::Local(_)), .. } if actor_name == "Counter")),
        "spawn Counter should lower with initialized state"
    );
}

#[test]
fn missing_non_defaulted_actor_state_field_still_requires_spawn_arg() {
    let pipeline = lower_module_from_source(
        r"
        actor Pair {
            var n: i64 = 7;
            var m: i64;
            receive fn ping() {}
        }

        fn main() -> i64 {
            spawn Pair;
            0
        }
        ",
    );

    assert!(
        pipeline.diagnostics.iter().any(|diag| {
            matches!(
                &diag.kind,
                MirDiagnosticKind::NotYetImplemented { construct, .. }
                    if construct.contains("spawn `Pair` missing field `m`")
            )
        }),
        "missing non-defaulted field should remain required; diagnostics: {:#?}",
        pipeline.diagnostics
    );
}
