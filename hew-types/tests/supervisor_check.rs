mod common;

use common::typecheck_isolated as typecheck;

// ── Accept cases ──────────────────────────────────────────────────────────────

/// A static supervisor with multiple children and one `wired_to` reference compiles cleanly.
#[test]
fn multi_child_with_wired_to_accepted() {
    let output = typecheck(
        r"
        actor DbSupervisor {}
        actor Broadcaster {}
        actor WorkerPool {}
        actor ConnectionAcceptor {
            init(workers: LocalPid<WorkerPool>, broadcaster: LocalPid<Broadcaster>) {}
        }
        actor MessageCache {}

        supervisor ChatApp {
            strategy: one_for_one
            intensity: 5 within 60s

            child db: DbSupervisor
            child broadcaster: Broadcaster
            child worker_pool: WorkerPool
            child acceptor: ConnectionAcceptor wired_to: { workers: worker_pool, broadcaster: broadcaster }
            child cache: MessageCache restart: transient
        }

        fn main() {}
        ",
    );
    let supervisor_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.message.contains("E_SUPERVISOR"))
        .collect();
    assert!(
        supervisor_errors.is_empty(),
        "valid supervisor with wired_to should not produce supervisor errors: {supervisor_errors:#?}"
    );
}

/// A `simple_one_for_one` supervisor with a single pool child is valid.
#[test]
fn simple_one_for_one_with_pool_accepted() {
    let output = typecheck(
        r"
        actor Worker {}

        supervisor WorkerPool {
            strategy: simple_one_for_one
            intensity: 10 within 60s

            pool worker: Worker
        }

        fn main() {}
        ",
    );
    let supervisor_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.message.contains("E_SUPERVISOR"))
        .collect();
    assert!(
        supervisor_errors.is_empty(),
        "simple_one_for_one + pool should be valid: {supervisor_errors:#?}"
    );
}

// ── await_restart keyword ────────────────────────────────────────────────────

/// `await_restart sup.child` on a STATIC supervised child type-checks cleanly:
/// the operand is a static child accessor, so the keyword is well-formed and the
/// re-fetched handle keeps the child's `LocalPid<ChildType>` type.
#[test]
fn await_restart_on_static_child_accepted() {
    let output = typecheck(
        r"
        actor Worker {
            receive fn ping() {}
        }

        supervisor App {
            strategy: one_for_one
            intensity: 3 within 60s

            child w: Worker
        }

        fn main() {
            let sup = spawn App;
            let _w = await_restart sup.w;
        }
        ",
    );
    let relevant: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.message.contains("await_restart"))
        .collect();
    assert!(
        relevant.is_empty(),
        "`await_restart sup.w` on a static child must type-check cleanly: {relevant:#?}"
    );
}

/// `await_restart` on a POOL member is rejected: a pool member has no per-slot
/// restart signal (the `restart_notify` is per-supervisor, and pool dynamics
/// recover via the pool path, not a static child slot).
#[test]
fn await_restart_on_pool_member_rejected() {
    let output = typecheck(
        r"
        actor Worker {
            receive fn ping() {}
        }

        supervisor Pool {
            strategy: simple_one_for_one
            intensity: 10 within 60s

            pool worker: Worker
        }

        fn main() {
            let sup = spawn Pool;
            let _w = await_restart sup.worker;
        }
        ",
    );
    let rejected = output.errors.iter().any(|e| {
        e.message.contains("await_restart") && e.message.contains("static supervised child")
    });
    assert!(
        rejected,
        "`await_restart` on a pool member must be rejected with a static-child diagnostic; \
         got: {:#?}",
        output.errors
    );
}

/// `await_restart` on a non-supervisor-child operand is rejected with a clear
/// diagnostic — the operand must name a supervisor child slot.
#[test]
fn await_restart_on_non_child_operand_rejected() {
    let output = typecheck(
        r"
        fn main() {
            let x = 5;
            let _y = await_restart x;
        }
        ",
    );
    let rejected = output.errors.iter().any(|e| {
        e.message.contains("await_restart") && e.message.contains("supervisor child slot")
    });
    assert!(
        rejected,
        "`await_restart` on a non-supervised-child operand must be rejected; got: {:#?}",
        output.errors
    );
}

// ── Reject: wired_to unknown sibling ─────────────────────────────────────────

/// `wired_to` referencing a name that is not a declared sibling is rejected.
#[test]
fn wired_to_unknown_sibling_rejected() {
    let output = typecheck(
        r"
        actor ConnectionAcceptor {
            init(workers: LocalPid<WorkerPool>) {}
        }
        actor WorkerPool {}

        supervisor App {
            strategy: one_for_one

            child acceptor: ConnectionAcceptor wired_to: { workers: nonexistent_sibling }
        }

        fn main() {}
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("E_SUPERVISOR_WIRED_TO_UNKNOWN_SIBLING")),
        "wired_to with unknown sibling should fail: {:#?}",
        output.errors
    );
}

// ── Reject: wired_to type mismatch ───────────────────────────────────────────

/// `wired_to` where the init param is typed for a different actor is rejected.
#[test]
fn wired_to_type_mismatch_rejected() {
    let output = typecheck(
        r"
        actor DbPool {}
        actor WorkerPool {}
        actor ConnectionAcceptor {
            init(workers: LocalPid<WorkerPool>) {}
        }

        supervisor App {
            strategy: one_for_one

            child db_pool: DbPool
            child acceptor: ConnectionAcceptor wired_to: { workers: db_pool }
        }

        fn main() {}
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("E_SUPERVISOR_WIRED_TO_TYPE_MISMATCH")),
        "wired_to type mismatch (DbPool vs WorkerPool) should fail: {:#?}",
        output.errors
    );
}

// ── Reject: dependency cycle ──────────────────────────────────────────────────

/// A two-child cycle (a -> b -> a) is rejected.
#[test]
fn wired_to_cycle_rejected() {
    let output = typecheck(
        r"
        actor ActorA {
            init(dep: LocalPid<ActorB>) {}
        }
        actor ActorB {
            init(dep: LocalPid<ActorA>) {}
        }

        supervisor CycleApp {
            strategy: one_for_one

            child a: ActorA wired_to: { dep: b }
            child b: ActorB wired_to: { dep: a }
        }

        fn main() {}
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("E_SUPERVISOR_WIRED_CYCLE")),
        "wired_to cycle a->b->a should fail: {:#?}",
        output.errors
    );
}

// ── Reject: simple_one_for_one + child decl ───────────────────────────────────

/// `simple_one_for_one` strategy with child decls (not pool) is rejected.
#[test]
fn simple_one_for_one_with_child_decl_rejected() {
    let output = typecheck(
        r"
        actor Worker {}
        actor Helper {}

        supervisor BadPool {
            strategy: simple_one_for_one

            child helper: Helper
            pool worker: Worker
        }

        fn main() {}
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("E_SUPERVISOR_STRATEGY_POOL_MISMATCH")),
        "simple_one_for_one with child decl should fail: {:#?}",
        output.errors
    );
}

// ── Reject: one_for_one + pool decl ──────────────────────────────────────────

/// A non-simple_one_for_one strategy with a pool child is rejected.
#[test]
fn one_for_one_with_pool_decl_rejected() {
    let output = typecheck(
        r"
        actor Worker {}

        supervisor StaticApp {
            strategy: one_for_one

            pool worker: Worker
        }

        fn main() {}
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("E_SUPERVISOR_STRATEGY_POOL_MISMATCH")),
        "one_for_one with pool decl should fail: {:#?}",
        output.errors
    );
}

// ── Reject: duplicate child names ─────────────────────────────────────────────

/// Two children with the same name are rejected.
#[test]
fn duplicate_child_name_rejected() {
    let output = typecheck(
        r"
        actor Worker {}

        supervisor DupApp {
            strategy: one_for_one

            child worker: Worker
            child worker: Worker
        }

        fn main() {}
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("E_SUPERVISOR_DUPLICATE_CHILD")),
        "duplicate child name should fail: {:#?}",
        output.errors
    );
}

// ── Reject: wired_to self-reference (degenerate cycle) ────────────────────────

/// A child that wires itself (`wired_to: { x: self_name }`) is a cycle and is rejected.
#[test]
fn wired_to_self_reference_rejected() {
    let output = typecheck(
        r"
        actor LoopActor {
            init(dep: LocalPid<LoopActor>) {}
        }

        supervisor SelfLoop {
            strategy: one_for_one

            child looper: LoopActor wired_to: { dep: looper }
        }

        fn main() {}
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("E_SUPERVISOR_WIRED_CYCLE")),
        "self-reference wired_to should fail as a cycle: {:#?}",
        output.errors
    );
}

// ── Accept: wired_to sibling that has no init block ───────────────────────────

/// Wiring to a sibling with no `init` block is valid when the dependent actor's
/// init param type matches `LocalPid<SiblingType>`. The sibling having no `init`
/// is irrelevant — only the dependent's `init` params are checked.
#[test]
fn wired_to_no_init_sibling_accepted() {
    let output = typecheck(
        r"
        actor NoInit {}
        actor Consumer {
            init(dep: LocalPid<NoInit>) {}
        }

        supervisor App {
            strategy: one_for_one

            child no_init: NoInit
            child consumer: Consumer wired_to: { dep: no_init }
        }

        fn main() {}
        ",
    );
    let supervisor_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.message.contains("E_SUPERVISOR"))
        .collect();
    assert!(
        supervisor_errors.is_empty(),
        "wired_to a no-init sibling with a matching LocalPid param should be valid: {supervisor_errors:#?}"
    );
}

// ── Reject: wired_to key names param from actor with no init ──────────────────

/// When the *dependent* actor has no `init` block, any `wired_to` key is rejected
/// because there is no parameter for it to map to.
#[test]
fn wired_to_dependent_has_no_init_rejected() {
    let output = typecheck(
        r"
        actor Helper {}
        actor NoInitConsumer {}

        supervisor App {
            strategy: one_for_one

            child helper: Helper
            child consumer: NoInitConsumer wired_to: { dep: helper }
        }

        fn main() {}
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.message.contains("E_SUPERVISOR_WIRED_TO_TYPE_MISMATCH")),
        "wired_to on no-init dependent actor should fail with type mismatch: {:#?}",
        output.errors
    );
}
