//! HIR coverage for supervisor child accessors.

use hew_hir::{lower_program, ResolutionCtx};
use hew_types::{module_registry::ModuleRegistry, Checker};

fn lower(source: &str) -> hew_hir::LowerOutput {
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
        "type-check errors: {:#?}",
        tc_output.errors
    );
    lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    )
}

#[test]
fn static_child_access_compiles_cleanly() {
    let output = lower(
        r"
        actor Worker {
            receive fn ping() {}
        }

        supervisor App {
            strategy: one_for_one,
            child worker: Worker
        }

        fn get_worker(app: LocalPid<App>) -> LocalPid<Worker> {
            app.worker
        }
        ",
    );
    assert!(output.diagnostics.is_empty(), "{:#?}", output.diagnostics);
    assert!(output.into_result().is_ok());
}

#[test]
fn pool_field_access_lowers_as_first_class_view() {
    let output = lower(
        r"
        actor Worker {
            receive fn ping() {}
        }

        supervisor Pool {
            strategy: simple_one_for_one,
            pool workers: Worker(count: 2)
        }

        fn inspect(sup: LocalPid<Pool>) -> i64 {
            let workers = sup.workers;
            let first = workers[0];
            let maybe = workers.get(1);
            let _ = first;
            let _ = maybe;
            workers.len()
        }
        ",
    );
    assert!(output.diagnostics.is_empty(), "{:#?}", output.diagnostics);
    assert!(output.into_result().is_ok());
}

#[test]
fn nested_supervisor_chained_accessor_lowers_cleanly() {
    let output = lower(
        r"
        actor Worker {
            receive fn ping() {}
        }

        supervisor SubSupervisor {
            strategy: one_for_one,
            child worker: Worker
        }

        supervisor RootSupervisor {
            strategy: one_for_one,
            child sub: SubSupervisor
        }

        fn get_nested_worker(root: LocalPid<RootSupervisor>) -> LocalPid<Worker> {
            root.sub.worker
        }
        ",
    );
    assert!(output.diagnostics.is_empty(), "{:#?}", output.diagnostics);
    assert!(output.into_result().is_ok());
}
