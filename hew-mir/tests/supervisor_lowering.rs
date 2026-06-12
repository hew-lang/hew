//! S-C / S-D slice tests for supervisor MIR lowering.
//!
//! Validates that `HirItem::Supervisor` produces a bootstrap function whose
//! body spawns each declared child in topological order, threading
//! `wired_to:` siblings through the existing `Instr::SpawnActor` substrate
//! as `init_args`. The supervisor itself is materialized as a
//! `SupervisorLayout` on the produced `IrPipeline`.
//!
//! S-D.1 routing tests: `spawn Sup` in user code routes to a
//! `Terminator::Call { App__bootstrap }` with a `LocalPid<Sup>`-typed
//! destination, and init-args are rejected with `NotYetImplemented`.

use std::collections::HashMap;

use hew_hir::{lower_program, HirDiagnosticKind, ResolutionCtx};
use hew_mir::{lower_hir_module, FunctionCallConv, Instr, MirDiagnosticKind, Place, Terminator};
use hew_types::{module_registry::ModuleRegistry, BuiltinType, Checker, ResolvedTy};

/// Lower a Hew source program to MIR, asserting no parse or HIR diagnostics.
fn lower_module_from_source(source: &str) -> hew_mir::IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    // Run the type checker so its protocol descriptors and supervisor
    // wiring side tables populate the HIR lowering boundary. Without this
    // pass, `actor` decls would have no protocol_descriptor and HIR
    // lowering would emit boundary diagnostics on the wired_to bridge.
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

/// Lower a Hew source program to MIR, permitting HIR diagnostics for
/// unresolved enum-variant paths (e.g. `CrashAction::Restart`).
///
/// Enum variants are v0.5 HIR-out-of-scope (`TypeBodyItem::Variant` is
/// not lowered into HIR items); the type checker pre-registers them via
/// `register_builtin_failure_surface` so type-checking succeeds, but HIR
/// lowering emits `UnresolvedSymbol` for any `A::B` path expression in
/// the body. Tests that only care about symbol/layout propagation — not
/// about the hook body's evaluation — use this helper.
fn lower_module_from_source_with_enum_variants(source: &str) -> hew_mir::IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
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
    // Filter out known v0.5 HIR limitation: enum-variant path expressions
    // (e.g. `CrashAction::Restart`) are not resolvable in HIR lowering
    // because `TypeBodyItem::Variant` is out of slice scope. Other
    // diagnostics still fail the test.
    let non_enum_diags: Vec<_> = hir
        .diagnostics
        .iter()
        .filter(|d| {
            !matches!(
                &d.kind,
                HirDiagnosticKind::UnresolvedSymbol { name } if name.contains("::")
            )
        })
        .collect();
    assert!(
        non_enum_diags.is_empty(),
        "unexpected HIR diagnostics (non-enum-variant): {non_enum_diags:#?}"
    );
    lower_hir_module(&hir.module)
}

#[test]
fn supervisor_three_children_with_wired_to_dep_emits_topo_ordered_spawn_sequence() {
    let pipeline = lower_module_from_source(
        r"
        actor Db    { receive fn exec() {} }
        actor Cache { receive fn query() {} }
        actor Web {
            init(db: LocalPid<Db>, cache: LocalPid<Cache>) {}
            receive fn serve() {}
        }

        supervisor App {
            child db: Db
            child cache: Cache
            child web: Web wired_to: { db: db, cache: cache }
        }
        ",
    );

    assert_eq!(
        pipeline.supervisor_layouts.len(),
        1,
        "one supervisor declared"
    );
    let app_layout = &pipeline.supervisor_layouts[0];
    assert_eq!(app_layout.name, "App");
    assert_eq!(app_layout.bootstrap_symbol, "App__bootstrap");
    assert_eq!(app_layout.children.len(), 3);

    let order: Vec<&str> = app_layout
        .children
        .iter()
        .map(|c| c.name.as_str())
        .collect();
    assert_eq!(
        order,
        vec!["db", "cache", "web"],
        "wired-to deps must spawn before dependents"
    );
    let web_layout = app_layout
        .children
        .iter()
        .find(|c| c.name == "web")
        .expect("web in supervisor layout");
    assert_eq!(web_layout.spawn_order, 2);
    assert_eq!(web_layout.actor_name, "Web");
    let mut expected_wired: HashMap<String, String> = HashMap::new();
    expected_wired.insert("db".to_string(), "db".to_string());
    expected_wired.insert("cache".to_string(), "cache".to_string());
    assert_eq!(web_layout.wired_to, expected_wired);

    let bootstrap = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "App__bootstrap")
        .expect("App__bootstrap emitted into raw_mir");
    // Default: the synthesised body is overridden by codegen (S-D.3), so the
    // bootstrap doesn't need to carry an execution-context arg. ActorHandler
    // would push one, breaking the 0-arg `Terminator::Call` from lower_spawn.
    assert_eq!(bootstrap.call_conv, FunctionCallConv::Default);
    assert_eq!(
        bootstrap.return_ty,
        ResolvedTy::named_builtin(
            "LocalPid",
            BuiltinType::LocalPid,
            vec![ResolvedTy::named_user("App", vec![])]
        ),
        "bootstrap returns LocalPid<App> so spawn-site callers receive a typed handle"
    );
    assert!(
        bootstrap.params.is_empty(),
        "the bootstrap is a no-arg synthetic entry-point"
    );

    let spawn_seq: Vec<(String, usize, Vec<Place>)> = bootstrap
        .blocks
        .iter()
        .flat_map(|b| b.instructions.iter())
        .filter_map(|instr| match instr {
            Instr::SpawnActor {
                actor_name,
                init_args,
                ..
            } => Some((actor_name.clone(), init_args.len(), init_args.clone())),
            _ => None,
        })
        .collect();

    assert_eq!(spawn_seq.len(), 3, "one SpawnActor per declared child");
    assert_eq!(spawn_seq[0].0, "Db");
    assert_eq!(spawn_seq[0].1, 0, "Db has no init args");
    assert_eq!(spawn_seq[1].0, "Cache");
    assert_eq!(spawn_seq[1].1, 0, "Cache has no init args");
    assert_eq!(spawn_seq[2].0, "Web");
    assert_eq!(
        spawn_seq[2].1, 2,
        "Web receives db + cache through wired_to init args"
    );

    for arg in &spawn_seq[2].2 {
        assert!(
            matches!(arg, Place::Local(_) | Place::ActorHandle(_)),
            "wired_to args must resolve to spawned-sibling handle locals; got {arg:?}"
        );
    }
}

#[test]
fn pool_supervisor_emits_layout_with_pool_flag() {
    let pipeline = lower_module_from_source(
        r"
        actor Worker { receive fn ping() {} }

        supervisor Pool {
            strategy: simple_one_for_one,
            pool worker: Worker
        }
        ",
    );
    assert_eq!(pipeline.supervisor_layouts.len(), 1);
    let pool = &pipeline.supervisor_layouts[0];
    assert_eq!(pool.name, "Pool");
    assert_eq!(pool.children.len(), 1);
    assert!(pool.children[0].is_pool);
    assert_eq!(pool.children[0].slot_index, 0);

    let bootstrap = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "Pool__bootstrap")
        .expect("Pool__bootstrap emitted");
    let spawn_count = bootstrap
        .blocks
        .iter()
        .flat_map(|b| b.instructions.iter())
        .filter(|i| matches!(i, Instr::SpawnActor { .. }))
        .count();
    assert_eq!(spawn_count, 1, "one spawn per declared child");
}

#[test]
fn supervisor_with_no_wired_to_preserves_declaration_order() {
    let pipeline = lower_module_from_source(
        r"
        actor A { receive fn ping() {} }
        actor B { receive fn ping() {} }
        actor C { receive fn ping() {} }

        supervisor S {
            child a: A
            child b: B
            child c: C
        }
        ",
    );
    let s = &pipeline.supervisor_layouts[0];
    let order: Vec<&str> = s.children.iter().map(|c| c.name.as_str()).collect();
    assert_eq!(
        order,
        vec!["a", "b", "c"],
        "siblings with no wired_to deps preserve source declaration order"
    );
}

/// S-D.1: `spawn App` in a user function routes to a `Terminator::Call` targeting
/// `App__bootstrap`, with a `LocalPid<App>`-typed destination place. No diagnostics.
#[test]
fn spawn_supervisor_routes_to_bootstrap_call_with_local_pid_return() {
    let pipeline = lower_module_from_source(
        r"
        actor Worker { receive fn ping() {} }

        supervisor App {
            child w: Worker
        }

        fn main() -> i64 {
            spawn App;
            42
        }
        ",
    );

    assert!(
        pipeline.diagnostics.is_empty(),
        "no MIR diagnostics expected; got: {:#?}",
        pipeline.diagnostics
    );

    let main_fn = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "main")
        .expect("main function emitted into raw_mir");

    // Find the Terminator::Call targeting App__bootstrap.
    let call_term = main_fn
        .blocks
        .iter()
        .find_map(|b| {
            if let Terminator::Call { callee, dest, .. } = &b.terminator {
                if callee == "App__bootstrap" {
                    return Some((callee.clone(), *dest));
                }
            }
            None
        })
        .expect("main body contains a Terminator::Call targeting App__bootstrap");

    // The destination place must be typed LocalPid<App>.
    let (_, dest) = call_term;
    let dest_place = dest.expect("bootstrap call must allocate a return destination");
    let local_idx = match dest_place {
        Place::Local(idx) => idx,
        other => panic!("expected Place::Local for bootstrap call dest, got {other:?}"),
    };
    let dest_ty = &main_fn.locals[local_idx as usize];
    assert_eq!(
        *dest_ty,
        ResolvedTy::named_builtin(
            "LocalPid",
            BuiltinType::LocalPid,
            vec![ResolvedTy::named_user("App", vec![])]
        ),
        "bootstrap call destination must have type LocalPid<App>"
    );

    // No SpawnActor instruction in main — the supervisor is called, not spawned directly.
    let spawn_count = main_fn
        .blocks
        .iter()
        .flat_map(|b| b.instructions.iter())
        .filter(|i| matches!(i, Instr::SpawnActor { .. }))
        .count();
    assert_eq!(
        spawn_count, 0,
        "main must not contain Instr::SpawnActor for a supervisor spawn"
    );
}

/// S-D.1 negative: `spawn App(x: 1)` emits `NotYetImplemented` with the
/// "supervisor spawn with init args" construct string.
#[test]
fn spawn_supervisor_with_init_args_emits_not_yet_implemented() {
    // Build the pipeline without asserting HIR or MIR diagnostics clean —
    // the MIR diagnostic we want is exactly what this test asserts.
    let parsed = hew_parser::parse(
        r"
        actor Worker { receive fn ping() {} }

        supervisor App {
            child w: Worker
        }

        fn main() -> i64 {
            spawn App(x: 1);
            42
        }
        ",
    );
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    let hir = hew_hir::lower_program(
        &parsed.program,
        &tc_output,
        &hew_hir::ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    let pipeline = hew_mir::lower_hir_module(&hir.module);

    let nyi_diag = pipeline.diagnostics.iter().find(|d| {
        matches!(
            &d.kind,
            MirDiagnosticKind::NotYetImplemented { construct, .. }
                if construct.contains("supervisor spawn with init args")
        )
    });
    assert!(
        nyi_diag.is_some(),
        "expected NotYetImplemented for supervisor spawn with init args; \
         got diagnostics: {:#?}",
        pipeline.diagnostics
    );
}

/// Supervisor child whose actor declares `#[on(crash)]` surfaces a
/// non-`None` `on_crash_symbol` on the matching `SupervisorChildLayout`.
/// A sibling child whose actor has no crash hook stays `None`.
#[test]
fn supervisor_child_with_on_crash_hook_surfaces_symbol_on_layout() {
    // Use the enum-variant-tolerating helper: `CrashAction::Restart` in the
    // hook body is unresolvable in HIR lowering (enum variants are v0.5
    // out-of-scope) but the type checker accepts it. The test only cares
    // that the MIR layout propagation is correct.
    let pipeline = lower_module_from_source_with_enum_variants(
        r"
        actor Crashable {
            receive fn ping() {}
            #[on(crash)]
            fn handle_crash(info: CrashInfo) -> CrashAction { CrashAction::Restart }
        }
        actor Stable {
            receive fn ping() {}
        }

        supervisor App {
            child crashable: Crashable
            child stable: Stable
        }
        ",
    );

    // `CrashInfo` and `CrashAction` are seeded into the HIR TypeClassTable by
    // `seed_builtin_type_classes`, so `push_unknown_type_diagnostics` no longer
    // fires for them. The hook body uses `CrashAction::Restart`, which HIR
    // lowers as `UnresolvedSymbol` (enum-variant paths are v0.5 out-of-scope),
    // but that diagnostic is filtered by the `lower_module_from_source_with_enum_variants`
    // helper above. No MIR diagnostics are expected here.
    let unexpected_diags: Vec<_> = pipeline.diagnostics.iter().collect();
    assert!(
        unexpected_diags.is_empty(),
        "unexpected MIR diagnostics: {unexpected_diags:#?}"
    );

    let app_layout = pipeline
        .supervisor_layouts
        .iter()
        .find(|s| s.name == "App")
        .expect("App supervisor layout must be present");

    let crashable_child = app_layout
        .children
        .iter()
        .find(|c| c.name == "crashable")
        .expect("crashable child in supervisor layout");
    assert_eq!(
        crashable_child.on_crash_symbol.as_deref(),
        Some("Crashable__on_crash"),
        "child whose actor has #[on(crash)] must carry the mangled symbol"
    );

    let stable_child = app_layout
        .children
        .iter()
        .find(|c| c.name == "stable")
        .expect("stable child in supervisor layout");
    assert_eq!(
        stable_child.on_crash_symbol, None,
        "child whose actor has no #[on(crash)] must have on_crash_symbol = None"
    );

    // Verify the MIR function for Crashable__on_crash was emitted.
    let crash_fn = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "Crashable__on_crash")
        .expect("Crashable__on_crash must be emitted into raw_mir");
    assert_eq!(
        crash_fn.call_conv,
        hew_mir::FunctionCallConv::ActorHandler,
        "on(crash) MIR function must use ActorHandler calling convention"
    );
}
