mod common;

use common::typecheck;

#[test]
fn pid_trait_generic_send_fails_closed_without_serializable_projection_bound() {
    let output = typecheck(
        r"
        record Work {
            id: i32,
        }

        actor Worker {
            let id: i32;
            init() {}
            receive fn handle(msg: Work) {}
        }

        impl ActorMsg for Worker {
            type Msg = Work;
            type Reply = ();
        }

        fn ping<P: Pid>(p: P, m: P::Msg) -> Result<(), SendError> {
            p.send(m)
        }

        fn main() {
            let worker = spawn Worker(id: 0);
            let msg = Work { id: 1 };
            let result: Result<(), SendError> = ping(worker, msg);
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.message.contains("fail-closed")
                && error.message.contains("P::Msg: Serializable")),
        "generic Pid::send must fail closed without a P::Msg Serializable proof: {:#?}",
        output.errors
    );
}

#[test]
fn local_pid_generic_pid_send_still_fails_closed_without_projection_bound() {
    let output = typecheck(
        r"
        record Job {
            n: i32,
        }

        actor Worker {
            let id: i32;
            init() {}
            receive fn run(job: Job) {}
        }

        impl ActorMsg for Worker {
            type Msg = Job;
            type Reply = ();
        }

        fn takes_pid<P: Pid>(pid: P, msg: P::Msg) -> Result<(), SendError> {
            pid.send(msg)
        }

        fn main() {
            let worker = spawn Worker(id: 7);
            let result: Result<(), SendError> = takes_pid(worker, Job { n: 3 });
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.message.contains("fail-closed")
                && error.message.contains("P::Msg: Serializable")),
        "generic Pid::send must fail closed even when a call site later supplies LocalPid: {:#?}",
        output.errors
    );
}

#[test]
fn local_pid_send_without_handler_rejected_even_with_actor_msg_envelope() {
    // #2367: an `impl ActorMsg for T` binding records a message-envelope
    // type but does not by itself wire local-mailbox delivery — there is
    // no receive fn resolved to accept the message. Previously admitted
    // here and failed closed later at HIR lowering with an internal
    // `MethodCallNoRewrite` diagnostic; now rejected uniformly with the
    // same actionable diagnostic as the no-envelope case.
    let output = typecheck(
        r"
        record Job {
            n: i32,
        }

        actor Worker {
            let id: i32;
            init() {}
            receive fn run(job: Job) {}
        }

        impl ActorMsg for Worker {
            type Msg = Job;
            type Reply = ();
        }

        fn main() {
            let worker = spawn Worker(id: 7);
            let result = worker.send(Job { n: 3 });
        }
        ",
    );
    assert!(
        output.errors.iter().any(|error| {
            error.kind == hew_types::error::TypeErrorKind::UndefinedMethod
                && error.message.contains("no `send` handler on `Worker`")
        }),
        "LocalPid.send with an ActorMsg envelope but no send handler must \
         be rejected at type-check with the same actionable diagnostic as \
         the no-envelope case: {:#?}",
        output.errors
    );
}

#[test]
fn remote_pid_send_returns_typed_send_error_stub() {
    let output = typecheck(
        r"
        record Job {
            n: i32,
        }

        actor Worker {
            let id: i32;
            init() {}
            receive fn run(job: Job) {}
        }

        impl ActorMsg for Worker {
            type Msg = Job;
            type Reply = i32;
        }

        fn main() {
            let remote: RemotePid<Worker>;
            let result: Result<(), SendError> = remote.send(Job { n: 9 });
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "RemotePid.send should return Result<(), SendError> from the typed stub: {:#?}",
        output.errors
    );
}

#[test]
fn remote_pid_ask_returns_typed_reply_or_ask_error() {
    let output = typecheck(
        r"
        record Job {
            n: i32,
        }

        actor Worker {
            let id: i32;
            init() {}
            receive fn run(job: Job) -> i64 { 21 }
        }

        impl ActorMsg for Worker {
            type Msg = Job;
            type Reply = i64;
        }

        fn main() {
            let remote: RemotePid<Worker>;
            let result: Result<i64, AskError> = remote.ask(Job { n: 9 }, 250);
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "RemotePid.ask should return Result<T::Reply, AskError>: {:#?}",
        output.errors
    );
}

#[test]
fn remote_pid_ask_rejects_nonserializable_reply() {
    let output = typecheck(
        r"
        fn inc(x: i64) -> i64 { x + 1 }

        record Job {
            n: i32,
        }

        actor Worker {
            let id: i32;
            init() {}
            receive fn run(job: Job) {}
        }

        impl ActorMsg for Worker {
            type Msg = Job;
            type Reply = fn(i64) -> i64;
        }

        fn main() {
            let remote: RemotePid<Worker>;
            let result = remote.ask(Job { n: 9 }, 250);
        }
        ",
    );
    assert!(
        output.errors.iter().any(|error| {
            error.message.contains("remote actor reply type")
                && error.message.contains("must implement Serializable")
        }),
        "RemotePid.ask must reject non-Serializable replies: {:#?}",
        output.errors
    );
}

#[test]
fn local_pid_to_remote_via_stub_compiles() {
    let output = typecheck(
        r#"
        actor Bot {
            let n: i32;
            init() {}
        }
        fn main() {
            let local = spawn Bot(n: 0);
            let _remote = local.to_remote_via("node-1");
        }
    "#,
    );
    let undefined = output
        .errors
        .iter()
        .any(|e| matches!(e.kind, hew_types::error::TypeErrorKind::UndefinedMethod));
    assert!(
        !undefined,
        "to_remote_via should be dispatched on LocalPid (no UndefinedMethod): {:#?}",
        output.errors
    );
}
