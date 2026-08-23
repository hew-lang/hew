use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{lower_hir_module, MirDiagnosticKind, Terminator};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn lower_checked(source: &str) -> hew_mir::IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let type_result = checker.check_program(&parsed.program);
    assert!(
        type_result.errors.is_empty(),
        "type errors: {:#?}",
        type_result.errors
    );
    let hir = lower_program(
        &parsed.program,
        &type_result,
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

fn assert_closure_suspension_rejected(source: &str) -> hew_mir::IrPipeline {
    let pipeline = lower_checked(source);
    let matching: Vec<_> = pipeline
        .diagnostics
        .iter()
        .filter(|diagnostic| {
            matches!(
                &diagnostic.kind,
                MirDiagnosticKind::NotYetImplemented { construct, .. }
                    if construct == "suspension inside a closure"
            )
        })
        .collect();
    assert_eq!(
        matching.len(),
        1,
        "the unsupported construct must have one stable root diagnostic: {:#?}",
        pipeline.diagnostics
    );

    let shim = pipeline
        .raw_mir
        .iter()
        .find(|function| function.name.starts_with("__hew_closure_invoke_main_"))
        .expect("closure invoke shim");
    assert!(
        shim.blocks
            .iter()
            .any(|block| matches!(block.terminator, Terminator::Suspend { .. })),
        "the diagnostic must be derived from the closure shim's actual suspension carrier"
    );
    pipeline
}

#[test]
fn await_in_closure_reports_suspension_diagnostic_before_codegen() {
    assert_closure_suspension_rejected(
        r"
actor Adder {
    receive fn add(a: i64, b: i64) -> i64 {
        a + b
    }
}

fn main() {
    let adder = spawn Adder();
    let calculate = || {
        match await adder.add(7, 8) {
            .Ok(value) => value,
            .Err(_error) => 0,
        }
    };
    println(calculate());
}
",
    );
}

#[test]
fn sleep_in_closure_reports_suspension_not_await_diagnostic() {
    let pipeline = assert_closure_suspension_rejected(
        r"
fn main() {
    let pause = || {
        sleep(1ms);
    };
    pause();
}
",
    );
    assert!(
        pipeline.diagnostics.iter().all(|diagnostic| {
            !matches!(
                &diagnostic.kind,
                MirDiagnosticKind::NotYetImplemented { construct, .. }
                    if construct.contains("await")
            )
        }),
        "sleep must not be diagnosed as await: {:#?}",
        pipeline.diagnostics
    );
}

#[test]
fn statement_level_await_outside_closure_remains_supported() {
    let pipeline = lower_checked(
        r"
actor Adder {
    receive fn add(a: i64, b: i64) -> i64 {
        a + b
    }
}

fn main() {
    let adder = spawn Adder();
    let _ = await adder.add(7, 8);
}
",
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "statement-level await must remain diagnostic-free: {:#?}",
        pipeline.diagnostics
    );
}
