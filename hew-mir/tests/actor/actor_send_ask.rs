use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{Instr, MirDiagnosticKind, Terminator};
use hew_types::{compute_default_msg_id, module_registry::ModuleRegistry, Checker};

fn lower_checked(source: &str) -> hew_mir::IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(tc_output.errors.is_empty(), "{:?}", tc_output.errors);
    let hir = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(hir.diagnostics.is_empty(), "{:?}", hir.diagnostics);
    hew_mir::lower_hir_module(&hir.module)
}

fn lower_for_mir_diagnostics(source: &str) -> hew_mir::IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(tc_output.errors.is_empty(), "{:?}", tc_output.errors);
    let hir = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(hir.diagnostics.is_empty(), "{:?}", hir.diagnostics);
    hew_mir::lower_hir_module(&hir.module)
}

#[test]
fn actor_send_ask_lower_to_mir_terminators_and_state_access() {
    let pipeline = lower_checked(
        r"
        actor Counter {
            var count: i64;

            receive fn increment(n: i64) {
                count = count + n;
            }

            receive fn total() -> i64 {
                count
            }
        }

        fn main() -> i64 {
            let counter = spawn Counter(count: 0);
            counter.increment(10);
            match await counter.total() {
                Ok(v) => v,
                Err(_e) => 0,
            }
        }
        ",
    );

    assert!(
        pipeline.diagnostics.is_empty(),
        "MIR diagnostics: {:?}",
        pipeline.diagnostics
    );
    // Q87 slice 1: msg_ids are no longer source-order indices — they are the
    // low 32 bits of `SipHasher13::new_with_keys(0, 0).write(<qualified name>)`.
    // The MIR layout reinterprets the u32 hash as an i32 (bit-preserving)
    // before threading it into `Send` / `Ask` terminators.
    let increment_id =
        i32::from_ne_bytes(compute_default_msg_id("Counter::increment").to_ne_bytes());
    let total_id = i32::from_ne_bytes(compute_default_msg_id("Counter::total").to_ne_bytes());
    assert_ne!(
        increment_id, total_id,
        "Counter::increment and Counter::total must hash to distinct msg_ids"
    );
    assert_eq!(pipeline.actor_layouts.len(), 1);
    assert_eq!(pipeline.actor_layouts[0].handlers[0].name, "increment");
    assert_eq!(pipeline.actor_layouts[0].handlers[0].msg_type, increment_id);
    assert_eq!(pipeline.actor_layouts[0].handlers[1].name, "total");
    assert_eq!(pipeline.actor_layouts[0].handlers[1].msg_type, total_id);

    let main = pipeline
        .raw_mir
        .iter()
        .find(|func| func.name == "main")
        .expect("main MIR");
    assert!(main
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .any(
            |instr| matches!(instr, Instr::SpawnActor { actor_name, .. } if actor_name == "Counter")
        ));
    assert!(main.blocks.iter().any(|block| {
        matches!(block.terminator, Terminator::Send { msg_type, .. } if msg_type == increment_id)
    }));
    assert!(main.blocks.iter().any(|block| {
        matches!(block.terminator, Terminator::Ask { msg_type, .. } if msg_type == total_id)
    }));

    let increment = pipeline
        .raw_mir
        .iter()
        .find(|func| func.name == "Counter__recv__increment")
        .expect("increment handler MIR");
    assert!(increment
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .any(|instr| matches!(instr, Instr::ActorStateFieldStore { .. })));
    let total = pipeline
        .raw_mir
        .iter()
        .find(|func| func.name == "Counter__recv__total")
        .expect("total handler MIR");
    assert!(total
        .blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .any(|instr| matches!(instr, Instr::ActorStateFieldLoad { .. })));
}

#[test]
fn spawn_init_actor_accepts_state_field_argument_coexist() {
    // COEXIST model: state-field spawn args are valid alongside init() params.
    // `spawn Counter(count: 5)` should lower cleanly — `count` is routed into
    // the initial state record; `init()` is still called but with no explicit
    // arg (the init param `initial` is missing here, so we check that the
    // *absence* of an init param surfaces, not the presence of a state-field arg).
    //
    // For a clean lower we supply both the state-field arg AND the init param.
    let pipeline = lower_checked(
        r"
        actor Counter {
            var count: i64;

            init(initial: i64) {
                count = initial;
            }

            receive fn total() -> i64 {
                count
            }
        }

        fn main() {
            let _counter = spawn Counter(initial: 0, count: 5);
        }
        ",
    );

    assert!(
        pipeline.diagnostics.is_empty(),
        "expected no MIR diagnostics for coexist spawn; got: {:#?}",
        pipeline.diagnostics
    );
}

#[test]
fn spawn_init_actor_rejects_unknown_argument() {
    // A name that is neither an init() parameter nor a state field must still
    // be rejected with a clear diagnostic.
    let pipeline = lower_for_mir_diagnostics(
        r"
        actor Counter {
            var count: i64;

            init(initial: i64) {
                count = initial;
            }

            receive fn total() -> i64 {
                count
            }
        }

        fn main() {
            let _counter = spawn Counter(initial: 0, bogus: 99);
        }
        ",
    );

    let diagnostic = pipeline
        .diagnostics
        .iter()
        .find(|d| {
            matches!(
                &d.kind,
                MirDiagnosticKind::InvalidActorSpawnArgument {
                    actor,
                    argument,
                    ..
                } if actor == "Counter" && argument == "bogus"
            )
        })
        .unwrap_or_else(|| {
            panic!(
                "expected InvalidActorSpawnArgument for `bogus`; diagnostics: {:#?}",
                pipeline.diagnostics
            )
        });
    assert!(
        diagnostic.note.contains("init() parameters are:") && diagnostic.note.contains("`initial`"),
        "note should list init params; got: {}",
        diagnostic.note
    );
    assert!(
        diagnostic.note.contains("state fields are:") && diagnostic.note.contains("`count`"),
        "note should list state fields; got: {}",
        diagnostic.note
    );
    assert!(
        !pipeline
            .diagnostics
            .iter()
            .any(|d| matches!(d.kind, MirDiagnosticKind::NotYetImplemented { .. })),
        "invalid spawn arg must not surface as NotYetImplemented: {:#?}",
        pipeline.diagnostics
    );
}

#[test]
fn remote_pid_ask_lowers_to_remote_ask_terminator() {
    let pipeline = lower_checked(
        r"
        record Ping {
            n: i64,
        }

        actor Worker {
            receive fn double(msg: Ping) -> i64 {
                msg.n * 2
            }
        }

        impl ActorMsg for Worker {
            type Msg = Ping;
            type Reply = i64;
        }

        fn main() -> i64 {
            let remote = RemotePid::<Worker>::from_raw(2, 7);
            let result: Result<i64, AskError> = remote.ask(Ping { n: 21 }, 250);
            0
        }
        ",
    );

    assert!(
        pipeline.diagnostics.is_empty(),
        "MIR diagnostics: {:?}",
        pipeline.diagnostics
    );
    let double_id = i32::from_ne_bytes(compute_default_msg_id("Worker::double").to_ne_bytes());
    let main = pipeline
        .raw_mir
        .iter()
        .find(|func| func.name == "main")
        .expect("main MIR");
    assert!(main.blocks.iter().any(|block| {
        matches!(
            &block.terminator,
            Terminator::RemoteAsk {
                msg_type,
                reply_ty: hew_types::ResolvedTy::I64,
                ..
            } if *msg_type == double_id
        )
    }));
}
