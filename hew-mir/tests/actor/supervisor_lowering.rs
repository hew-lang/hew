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
            pool worker: Worker(count: 2)
        }
        ",
    );
    assert_eq!(pipeline.supervisor_layouts.len(), 1);
    let pool = &pipeline.supervisor_layouts[0];
    assert_eq!(pool.name, "Pool");
    assert_eq!(pool.children.len(), 1);
    assert!(pool.children[0].is_pool);
    assert_eq!(pool.children[0].slot_index, 0);
    // The reserved `count: 2` lowers to a literal pool size.
    assert_eq!(
        pool.children[0].pool_count,
        Some(hew_mir::PoolCount::Literal(2)),
        "the pool count must lower to a literal 2"
    );

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

/// Negative: spawning a NO-config supervisor with init args
/// (`spawn App(x: 1)` where `App` declares no `(config: T)`) is rejected at the
/// HIR supervisor-spawn gate. Config supervisors admit exactly their config arg
/// (the init-closure model); a no-config supervisor admits none.
#[test]
fn spawn_no_config_supervisor_with_init_args_is_rejected_at_hir() {
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

    let gate_diag = hir.diagnostics.iter().find(|d| {
        matches!(
            &d.kind,
            HirDiagnosticKind::SupervisorSpawnArgsUnsupported { supervisor_name }
                if supervisor_name == "App"
        )
    });
    assert!(
        gate_diag.is_some(),
        "expected SupervisorSpawnArgsUnsupported for a no-config supervisor spawned with args; \
         got HIR diagnostics: {:#?}",
        hir.diagnostics
    );
}

/// A config supervisor spawned with its config arg
/// (`spawn App(config: cfg)`) is ADMITTED through the HIR gate and lowers the
/// bootstrap call with the config value threaded as the bootstrap's argument.
#[test]
fn spawn_config_supervisor_admits_config_arg_and_threads_it_to_bootstrap() {
    let pipeline = lower_module_from_source(
        r"
        record AppConfig { size: i64 }

        actor Cache {
            var capacity: i64;
            receive fn get_cap() -> i64 { capacity }
        }

        supervisor App(config: AppConfig) {
            child cache: Cache(capacity: config.size)
        }

        fn main() -> i64 {
            let cfg = AppConfig { size: 7 };
            let sup = spawn App(config: cfg);
            0
        }
        ",
    );

    // The supervisor layout carries the config param.
    let app = pipeline
        .supervisor_layouts
        .iter()
        .find(|s| s.name == "App")
        .expect("App supervisor layout");
    let config_param = app
        .config_param
        .as_ref()
        .expect("config supervisor must carry a config param on its layout");
    assert_eq!(config_param.config_ty_name, "AppConfig");

    // The `spawn App(config: cfg)` site lowers to a bootstrap Call carrying one
    // argument (the config value) — not a zero-arg call.
    let main_fn = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "main")
        .expect("main emitted into raw_mir");
    let bootstrap_call_args = main_fn
        .blocks
        .iter()
        .find_map(|b| match &b.terminator {
            Terminator::Call { callee, args, .. } if callee == "App__bootstrap" => Some(args.len()),
            _ => None,
        })
        .expect("main must call App__bootstrap");
    assert_eq!(
        bootstrap_call_args, 1,
        "spawn App(config: cfg) must thread exactly the config value to the bootstrap"
    );

    // The bootstrap fn declares the config param so its codegen-real body can
    // read it.
    let bootstrap = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "App__bootstrap")
        .expect("App__bootstrap emitted into raw_mir");
    assert_eq!(
        bootstrap.params.len(),
        1,
        "the config bootstrap must declare exactly its config parameter"
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

/// Verify-AST gate (S1): a SCALAR config-field init arg
/// (`child cache: Cache(capacity: config.size)`) reaches MIR as a fully
/// resolved `ChildInitArg::ConfigField { owned: false, config_ty_name, field_ty
/// }` — the discriminator codegen reads to emit the init thunk. Without the
/// checker binding the config params in scope and synthesising the init-arg
/// expression types, `object.ty` would not be `Named{config_ty}` and the MIR
/// arm would NYI; this test fails closed if that regresses.
#[test]
fn scalar_config_field_init_arg_lowers_to_resolved_config_field() {
    use hew_mir::ChildInitArg;

    let pipeline = lower_module_from_source(
        r"
        record AppConfig { size: i64 }

        actor Cache {
            var capacity: i64;
            init(capacity: i64) {
                capacity = capacity;
            }
            receive fn get_cap() -> i64 { capacity }
        }

        supervisor App(config: AppConfig) {
            strategy: one_for_one;
            child cache: Cache(capacity: config.size)
        }
        ",
    );

    let app = pipeline
        .supervisor_layouts
        .iter()
        .find(|s| s.name == "App")
        .expect("App supervisor layout");
    let cache = app
        .children
        .iter()
        .find(|c| c.name == "cache")
        .expect("cache child layout");
    let (field_name, init_arg) = cache
        .init_state_fields
        .iter()
        .find(|(f, _)| f == "capacity")
        .expect("capacity init field must be present on the cache child layout");
    assert_eq!(field_name, "capacity");
    match init_arg {
        ChildInitArg::ConfigField {
            config_ty_name,
            field_name,
            field_ty,
            owned,
            ..
        } => {
            assert_eq!(
                config_ty_name, "AppConfig",
                "config object type must resolve to the config struct name"
            );
            assert_eq!(field_name, "size");
            assert_eq!(
                *field_ty,
                ResolvedTy::I64,
                "scalar config field type must be the resolved field type, not a default"
            );
            assert!(!owned, "an i64 config field is a scalar load, not owned");
        }
        other => panic!("expected ConfigField init arg, got {other:?}"),
    }
}

/// An OWNED `string` config field (`config.label: string`) lowers to a
/// `ChildInitArg::ConfigField { owned: true }` — the discriminator the codegen
/// init thunk reads to deep-clone the field per incarnation.
#[test]
fn owned_string_config_field_init_arg_lowers_to_owned_config_field() {
    use hew_mir::ChildInitArg;

    let pipeline = lower_module_from_source(
        r"
        record AppConfig { label: string }

        actor Tagged {
            let name: string;
            init(name: string) {
                name = name;
            }
            receive fn read() {}
        }

        supervisor App(config: AppConfig) {
            child t: Tagged(name: config.label)
        }
        ",
    );

    let app = pipeline
        .supervisor_layouts
        .iter()
        .find(|s| s.name == "App")
        .expect("App supervisor layout");
    let child = app
        .children
        .iter()
        .find(|c| c.name == "t")
        .expect("t child layout");
    let (_, init_arg) = child
        .init_state_fields
        .iter()
        .find(|(f, _)| f == "name")
        .expect("name init field present");
    match init_arg {
        ChildInitArg::ConfigField {
            field_ty, owned, ..
        } => {
            assert!(owned, "an owned `string` config field must lower as owned");
            assert_eq!(*field_ty, ResolvedTy::String);
        }
        other => panic!("expected an owned ConfigField init arg, got {other:?}"),
    }
}

/// An owned COLLECTION config field (`config.items: Vec<i64>`) stays walled at
/// the checker — the per-field collection clone-in-thunk is not yet wired, so it
/// fails closed rather than reaching a codegen path that cannot deep-clone it.
#[test]
fn owned_collection_config_field_init_arg_is_walled_at_checker() {
    let parsed = hew_parser::parse(
        r"
        record AppConfig { items: Vec<i64> }

        actor Holder {
            let data: Vec<i64>;
            init(data: Vec<i64>) {
                data = data;
            }
            receive fn read() {}
        }

        supervisor App(config: AppConfig) {
            child h: Holder(data: config.items)
        }
        ",
    );
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output
            .errors
            .iter()
            .any(|e| e.message.contains("E_SUPERVISOR_INIT_ARG_NON_BITCOPY")),
        "an owned `Vec` config init arg must stay walled until its clone-in-thunk codegen lands; \
         errors: {:#?}",
        tc_output.errors
    );
}

/// A supervisor `child` init arg naming a field the actor does not declare
/// reports exactly one `InvalidActorSpawnArgument` for the bad field — the
/// fail-open guard for the S4 delete: removing the supervisor-loop's redundant
/// unknown-field check must not drop the only diagnostic for this mistake, and
/// the correct `InvalidActorSpawnArgument` (already fired by the direct-spawn
/// validator the synthesized child spawn routes through) must remain the sole
/// report.
#[test]
fn supervisor_child_unknown_field_reports_exactly_one_invalid_actor_spawn_argument() {
    let pipeline = lower_module_from_source(
        r"
        actor Worker {
            let id: i64;
            receive fn work(x: i64) -> i64 { x }
        }

        supervisor WorkerPool {
            strategy: one_for_one;
            intensity: 5 within 10s;
            child w1: Worker(idx: 1)
        }
        ",
    );

    let invalid_matches: Vec<_> = pipeline
        .diagnostics
        .iter()
        .filter(|diag| {
            matches!(
                &diag.kind,
                MirDiagnosticKind::InvalidActorSpawnArgument { actor, argument, .. }
                    if actor == "Worker" && argument == "idx"
            )
        })
        .collect();
    assert_eq!(
        invalid_matches.len(),
        1,
        "supervisor child unknown field must report exactly one \
         InvalidActorSpawnArgument for the bad field; diagnostics: {:#?}",
        pipeline.diagnostics
    );
    assert!(
        !pipeline
            .diagnostics
            .iter()
            .any(|diag| matches!(&diag.kind, MirDiagnosticKind::NotYetImplemented { .. })),
        "an unknown supervisor child init-arg field name is a fully-understood user error and \
         must never surface as NotYetImplemented; diagnostics: {:#?}",
        pipeline.diagnostics
    );
}

/// A supervisor `child` missing a required actor state field (no init block,
/// no declared default) reports `MissingActorSpawnArgument` — a user error the
/// compiler fully understands — never `NotYetImplemented`.
#[test]
fn supervisor_child_missing_required_field_reports_missing_actor_spawn_argument() {
    let pipeline = lower_module_from_source(
        r"
        actor Worker {
            let id: i64;
            receive fn work(x: i64) -> i64 { x }
        }

        supervisor WorkerPool {
            strategy: one_for_one;
            intensity: 5 within 10s;
            child w1: Worker()
        }
        ",
    );

    assert!(
        pipeline.diagnostics.iter().any(|diag| {
            matches!(
                &diag.kind,
                MirDiagnosticKind::MissingActorSpawnArgument { actor, field, .. }
                    if actor == "Worker" && field == "id"
            )
        }),
        "supervisor child missing a required state field must report \
         MissingActorSpawnArgument; diagnostics: {:#?}",
        pipeline.diagnostics
    );
    let missing_matches: Vec<_> = pipeline
        .diagnostics
        .iter()
        .filter(|diag| {
            matches!(
                &diag.kind,
                MirDiagnosticKind::MissingActorSpawnArgument { actor, field, .. }
                    if actor == "Worker" && field == "id"
            )
        })
        .collect();
    assert_eq!(
        missing_matches.len(),
        1,
        "supervisor child missing a required state field must report exactly one \
         MissingActorSpawnArgument — the post-loop layout pass and the synthesized \
         spawn used to both independently diagnose the same field; diagnostics: {:#?}",
        pipeline.diagnostics
    );
    assert!(
        !pipeline
            .diagnostics
            .iter()
            .any(|diag| matches!(&diag.kind, MirDiagnosticKind::NotYetImplemented { .. })),
        "a missing supervisor child field is a fully-understood user error and must never \
         surface as NotYetImplemented; diagnostics: {:#?}",
        pipeline.diagnostics
    );
}

/// A config-backed supervisor child (any init arg reads `config.field`) is
/// entirely skipped by the synthesized-spawn machinery — the codegen init
/// thunk builds its real spawn from `SupervisorChildLayout` instead. Before
/// this fix, that skip ran BEFORE any field-name validation, so an unknown
/// field name on such a child passed silently: the (now-deleted)
/// supervisor-loop guard used to catch it, and the synthesized spawn never
/// even ran on a config-backed child to catch it either. This is the
/// fail-open regression the S4 delete introduced for config children
/// specifically.
///
/// An unknown arg name must report EXACTLY ONE diagnostic total — not the
/// `InvalidActorSpawnArgument` plus a cascaded `MissingActorSpawnArgument` for
/// the field the bad name was presumably meant to supply. Name validation
/// runs before missing-field planning and skips that planning entirely once
/// an invalid name is found, so `capacity` never gets flagged as missing
/// alongside `bogus` being flagged as invalid.
#[test]
fn supervisor_config_child_unknown_field_reports_invalid_actor_spawn_argument() {
    let pipeline = lower_module_from_source(
        r"
        record AppConfig { size: i64 }

        actor Cache {
            var capacity: i64;
            init(capacity: i64) {
                capacity = capacity;
            }
            receive fn get_cap() -> i64 { capacity }
        }

        supervisor App(config: AppConfig) {
            strategy: one_for_one;
            child cache: Cache(bogus: config.size)
        }
        ",
    );

    let relevant_matches: Vec<_> = pipeline
        .diagnostics
        .iter()
        .filter(|diag| {
            matches!(
                &diag.kind,
                MirDiagnosticKind::InvalidActorSpawnArgument { actor, .. }
                    if actor == "Cache"
            ) || matches!(
                &diag.kind,
                MirDiagnosticKind::MissingActorSpawnArgument { actor, .. }
                    if actor == "Cache"
            )
        })
        .collect();
    assert_eq!(
        relevant_matches.len(),
        1,
        "an unknown field name on a config-backed supervisor child must report exactly \
         one diagnostic total — InvalidActorSpawnArgument for the bad name, and NO \
         cascaded MissingActorSpawnArgument for the field it was meant to supply; \
         capabilities: hew_mir::ModuleCapabilities::EMPTY,
         diagnostics: {:#?}",
        pipeline.diagnostics
    );
    assert!(
        matches!(
            &relevant_matches[0].kind,
            MirDiagnosticKind::InvalidActorSpawnArgument { argument, .. } if argument == "bogus"
        ),
        "the one diagnostic must be InvalidActorSpawnArgument for `bogus`; diagnostics: {:#?}",
        pipeline.diagnostics
    );
    assert!(
        !pipeline
            .diagnostics
            .iter()
            .any(|diag| matches!(&diag.kind, MirDiagnosticKind::NotYetImplemented { .. })),
        "an unknown field name is a fully-understood user error and must never surface as \
         NotYetImplemented; diagnostics: {:#?}",
        pipeline.diagnostics
    );
}

/// A config-backed child that supplies one required field with a VALID name
/// but omits another required field entirely must still report
/// `MissingActorSpawnArgument` for the omitted field — name validation must
/// not blanket-suppress genuine missing-field diagnostics for config
/// children, only the cascade from an actually-invalid name.
#[test]
fn supervisor_config_child_valid_arg_missing_other_field_reports_missing_actor_spawn_argument() {
    let pipeline = lower_module_from_source(
        r"
        record AppConfig { size: i64 }

        actor Cache {
            var capacity: i64;
            var ttl: i64;
            init(capacity: i64, ttl: i64) {
                capacity = capacity;
                ttl = ttl;
            }
            receive fn get_cap() -> i64 { capacity }
        }

        supervisor App(config: AppConfig) {
            strategy: one_for_one;
            child cache: Cache(capacity: config.size)
        }
        ",
    );

    let missing_matches: Vec<_> = pipeline
        .diagnostics
        .iter()
        .filter(|diag| {
            matches!(
                &diag.kind,
                MirDiagnosticKind::MissingActorSpawnArgument { actor, field, .. }
                    if actor == "Cache" && field == "ttl"
            )
        })
        .collect();
    assert_eq!(
        missing_matches.len(),
        1,
        "a config-backed child supplying `capacity` with a valid name but omitting `ttl` \
         must still report exactly one MissingActorSpawnArgument for `ttl`; \
         diagnostics: {:#?}",
        pipeline.diagnostics
    );
    assert!(
        !pipeline.diagnostics.iter().any(|diag| matches!(
            &diag.kind,
            MirDiagnosticKind::InvalidActorSpawnArgument { .. }
        )),
        "the supplied `capacity` name is valid; no InvalidActorSpawnArgument should fire; \
         diagnostics: {:#?}",
        pipeline.diagnostics
    );
}
