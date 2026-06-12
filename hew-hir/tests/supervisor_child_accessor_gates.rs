//! Lane FC-P1-C: HIR fail-closed gates for unsupported supervisor child
//! accessor patterns.
//!
//! Two diagnostic kinds are exercised here:
//!
//! - `SupervisorPoolChildAccessorUnsupported` — `sup.pool_child` field access
//!   where the named child was declared with `pool name: Type`. Pool slot
//!   routing requires the `hew_supervisor_pool_route` ABI call which lands in
//!   v0.6. Currently fail-closed at `hew-mir/src/lower.rs:4413` as
//!   `NotYetImplemented`; this gate moves the rejection to HIR pre-pass so
//!   the program never reaches MIR.
//!
//! - `NestedSupervisorAccessorUnsupported` — `sup.nested` field access where
//!   the named child is itself a supervisor. Multi-segment supervisor dotted
//!   access requires the `hew_supervisor_nested_get` ABI call which lands in
//!   v0.6. Currently fail-closed at `hew-mir/src/lower.rs:4443` as
//!   `NotYetImplemented`; this gate moves the rejection to HIR pre-pass.
//!
//! Positive coverage confirms static (non-pool, non-nested) child access
//! continues to compile cleanly.

use hew_hir::{lower_program, HirDiagnosticKind, ResolutionCtx};
use hew_types::{module_registry::ModuleRegistry, Checker};

/// Parse, type-check, and HIR-lower a Hew source program at the host target
/// architecture. Asserts parsing succeeds; returns the HIR `LowerOutput` for
/// inspection of diagnostics + `into_result()` shape.
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

// ── Positive cases ───────────────────────────────────────────────────────────

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
    let has_pool_diag = output.diagnostics.iter().any(|d| {
        matches!(
            d.kind,
            HirDiagnosticKind::SupervisorPoolChildAccessorUnsupported { .. }
        )
    });
    let has_nested_diag = output.diagnostics.iter().any(|d| {
        matches!(
            d.kind,
            HirDiagnosticKind::NestedSupervisorAccessorUnsupported { .. }
        )
    });
    assert!(
        !has_pool_diag && !has_nested_diag,
        "static child access must not trigger pool/nested gate; diagnostics: {:#?}",
        output.diagnostics
    );
    assert!(
        output.into_result().is_ok(),
        "static child access must lower successfully"
    );
}

#[test]
fn multiple_static_children_compile_cleanly() {
    let output = lower(
        r"
        actor Worker {
            receive fn ping() {}
        }

        actor Logger {
            receive fn log() {}
        }

        supervisor App {
            strategy: one_for_one,
            child worker: Worker,
            child logger: Logger
        }

        fn setup(app: LocalPid<App>) {
            let w = app.worker;
            let l = app.logger;
            let _ = w;
            let _ = l;
        }
        ",
    );
    let has_gate_diag = output.diagnostics.iter().any(|d| {
        matches!(
            d.kind,
            HirDiagnosticKind::SupervisorPoolChildAccessorUnsupported { .. }
                | HirDiagnosticKind::NestedSupervisorAccessorUnsupported { .. }
        )
    });
    assert!(
        !has_gate_diag,
        "multiple static child accesses must not trigger any gate; diagnostics: {:#?}",
        output.diagnostics
    );
    assert!(output.into_result().is_ok());
}

// ── Negative case: pool child accessor ───────────────────────────────────────

#[test]
fn pool_child_access_rejected_at_hir() {
    let output = lower(
        r"
        actor Worker {
            receive fn ping() {}
        }

        supervisor Pool {
            strategy: simple_one_for_one,
            pool worker: Worker
        }

        fn get_pool_worker(sup: LocalPid<Pool>) -> LocalPid<Worker> {
            sup.worker
        }
        ",
    );
    let pool_diag = output.diagnostics.iter().find(|d| {
        matches!(
            d.kind,
            HirDiagnosticKind::SupervisorPoolChildAccessorUnsupported { .. }
        )
    });
    assert!(
        pool_diag.is_some(),
        "pool child access must emit SupervisorPoolChildAccessorUnsupported; diagnostics: {:#?}",
        output.diagnostics
    );
    // Confirm the diagnostic carries the expected supervisor + child names.
    if let Some(d) = pool_diag {
        if let HirDiagnosticKind::SupervisorPoolChildAccessorUnsupported { supervisor, child } =
            &d.kind
        {
            assert_eq!(supervisor, "Pool", "supervisor name preserved");
            assert_eq!(child, "worker", "child name preserved");
        }
    }
    // The fatal-set wiring must reject the program.
    assert!(
        output.into_result().is_err(),
        "pool child accessor diagnostic must be fatal (into_result() Err)"
    );
}

// ── Negative case: nested supervisor accessor ────────────────────────────────

#[test]
fn nested_supervisor_accessor_rejected_at_hir() {
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

        fn get_sub(root: LocalPid<RootSupervisor>) -> LocalPid<SubSupervisor> {
            root.sub
        }
        ",
    );
    let nested_diag = output.diagnostics.iter().find(|d| {
        matches!(
            d.kind,
            HirDiagnosticKind::NestedSupervisorAccessorUnsupported { .. }
        )
    });
    assert!(
        nested_diag.is_some(),
        "nested supervisor accessor must emit NestedSupervisorAccessorUnsupported; \
         diagnostics: {:#?}",
        output.diagnostics
    );
    if let Some(d) = nested_diag {
        if let HirDiagnosticKind::NestedSupervisorAccessorUnsupported {
            supervisor,
            child,
            nested_supervisor,
        } = &d.kind
        {
            assert_eq!(supervisor, "RootSupervisor");
            assert_eq!(child, "sub");
            assert_eq!(nested_supervisor, "SubSupervisor");
        }
    }
    assert!(
        output.into_result().is_err(),
        "nested supervisor accessor diagnostic must be fatal (into_result() Err)"
    );
}

#[test]
fn nested_supervisor_chained_accessor_rejected_at_first_hop() {
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
    // The first hop `root.sub` is a nested-supervisor access and MUST fire.
    let nested_diag = output.diagnostics.iter().find(|d| {
        matches!(
            d.kind,
            HirDiagnosticKind::NestedSupervisorAccessorUnsupported { .. }
        )
    });
    assert!(
        nested_diag.is_some(),
        "chained `root.sub.worker` must trigger nested-supervisor gate on the \
         first hop; diagnostics: {:#?}",
        output.diagnostics
    );
    assert!(
        output.into_result().is_err(),
        "chained nested-supervisor access must be fatal"
    );
}

// ── Disambiguation: two children of the same actor type ──────────────────────

#[test]
fn pool_child_accessor_disambiguated_with_two_same_type_children() {
    // `simple_one_for_one` constrains each supervisor to exactly one pool
    // child, so we exercise disambiguation across two supervisors whose
    // pool children share the same actor type. Accessing `sup_a.worker`
    // must produce a diagnostic naming supervisor `PoolA` and child
    // `worker` — NOT supervisor `PoolB`, which is what the pre-revision
    // reverse-by-type lookup would arbitrarily return when multiple
    // (sup, child) pairs match `slot.child_ty == "Worker"`.
    let output = lower(
        r"
        actor Worker {
            receive fn ping() {}
        }

        supervisor PoolA {
            strategy: simple_one_for_one,
            pool worker: Worker
        }

        supervisor PoolB {
            strategy: simple_one_for_one,
            pool worker: Worker
        }

        fn route(sup_a: LocalPid<PoolA>, sup_b: LocalPid<PoolB>) -> LocalPid<Worker> {
            sup_b.worker;
            sup_a.worker
        }
        ",
    );
    let pool_diags: Vec<_> = output
        .diagnostics
        .iter()
        .filter_map(|d| match &d.kind {
            HirDiagnosticKind::SupervisorPoolChildAccessorUnsupported { supervisor, child } => {
                Some((supervisor.clone(), child.clone()))
            }
            _ => None,
        })
        .collect();
    assert_eq!(
        pool_diags.len(),
        2,
        "one pool gate diagnostic per access expected; got: {pool_diags:?}"
    );
    // Both supervisors must appear, each exactly once — confirming the gate
    // attributes each access to its own supervisor rather than collapsing
    // them under whichever (sup, child) the reverse-by-type lookup hit first.
    assert!(
        pool_diags
            .iter()
            .any(|(s, c)| s == "PoolA" && c == "worker"),
        "expected diagnostic for PoolA.worker; got: {pool_diags:?}"
    );
    assert!(
        pool_diags
            .iter()
            .any(|(s, c)| s == "PoolB" && c == "worker"),
        "expected diagnostic for PoolB.worker; got: {pool_diags:?}"
    );
    assert!(
        output.into_result().is_err(),
        "pool child accessor diagnostics must be fatal"
    );
}

#[test]
fn nested_supervisor_accessor_disambiguated_with_two_same_type_subs() {
    // Two nested-supervisor children of the same supervisor type. Accessing
    // `sub_a` must name `sub_a`, not `sub_b`.
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
            child sub_a: SubSupervisor,
            child sub_b: SubSupervisor
        }

        fn get_sub_a(root: LocalPid<RootSupervisor>) -> LocalPid<SubSupervisor> {
            root.sub_a
        }
        ",
    );
    let nested_diags: Vec<_> = output
        .diagnostics
        .iter()
        .filter_map(|d| match &d.kind {
            HirDiagnosticKind::NestedSupervisorAccessorUnsupported {
                supervisor,
                child,
                nested_supervisor,
            } => Some((supervisor.clone(), child.clone(), nested_supervisor.clone())),
            _ => None,
        })
        .collect();
    assert_eq!(
        nested_diags.len(),
        1,
        "exactly one nested gate diagnostic expected; got: {nested_diags:?}"
    );
    let (sup, child, nested) = &nested_diags[0];
    assert_eq!(sup, "RootSupervisor");
    assert_eq!(
        child, "sub_a",
        "diagnostic must name the accessed child `sub_a`, NOT the sibling \
         `sub_b`; reverse-by-type lookup would mis-attribute"
    );
    assert_eq!(nested, "SubSupervisor");
    assert!(
        output.into_result().is_err(),
        "nested supervisor accessor diagnostic must be fatal"
    );
}
