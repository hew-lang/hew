mod common;

use common::typecheck;
use hew_types::Ty;

#[test]
fn pid_trait_generic_tell_fails_closed_without_serializable_projection_bound() {
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
            p.tell(m)
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
        "generic Pid::tell must fail closed without a P::Msg Serializable proof: {:#?}",
        output.errors
    );
}

#[test]
fn local_pid_generic_pid_tell_still_fails_closed_without_projection_bound() {
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
            pid.tell(msg)
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
        "generic Pid::tell must fail closed even when a call site later supplies LocalPid: {:#?}",
        output.errors
    );
}

#[test]
fn local_pid_tell_returns_result_send_error() {
    let (prog, output) = common::parse_and_typecheck_inline(
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
            let result = worker.tell(Job { n: 3 });
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "LocalPid.tell should type-check cleanly: {:#?}",
        output.errors
    );
    let main = prog
        .items
        .iter()
        .find_map(|(item, _)| match item {
            hew_parser::ast::Item::Function(fd) if fd.name == "main" => Some(fd),
            _ => None,
        })
        .expect("no main");
    let tell_span = main
        .body
        .stmts
        .iter()
        .find_map(|(stmt, _)| match stmt {
            hew_parser::ast::Stmt::Let {
                value: Some((hew_parser::ast::Expr::MethodCall { method, .. }, span)),
                ..
            } if method == "tell" => Some(span.clone()),
            _ => None,
        })
        .expect("no tell call");
    let ty = output
        .expr_types
        .get(&hew_types::check::SpanKey::from(&tell_span))
        .expect("tell expression type missing");
    assert_eq!(ty, &Ty::result(Ty::Unit, Ty::send_error()));
}

#[test]
fn remote_pid_tell_returns_typed_send_error_stub() {
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
            let result: Result<(), SendError> = remote.tell(Job { n: 9 });
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "RemotePid.tell should return Result<(), SendError> from the typed stub: {:#?}",
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
