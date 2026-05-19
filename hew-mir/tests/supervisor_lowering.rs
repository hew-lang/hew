//! S-C slice tests for supervisor MIR lowering.
//!
//! Validates that `HirItem::Supervisor` produces a bootstrap function whose
//! body spawns each declared child in topological order, threading
//! `wired_to:` siblings through the existing `Instr::SpawnActor` substrate
//! as `init_args`. The supervisor itself is materialized as a
//! `SupervisorLayout` on the produced `IrPipeline`.

use std::collections::HashMap;

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{lower_hir_module, FunctionCallConv, Instr, Place};
use hew_types::{module_registry::ModuleRegistry, Checker};

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
    let hir = lower_program(&parsed.program, &tc_output, &ResolutionCtx);
    assert!(
        hir.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        hir.diagnostics
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
    assert_eq!(
        bootstrap.call_conv,
        FunctionCallConv::ActorHandler,
        "supervisors are actor-likes: their bootstrap carries an execution context"
    );
    assert_eq!(bootstrap.return_ty, hew_types::ResolvedTy::Unit);
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
