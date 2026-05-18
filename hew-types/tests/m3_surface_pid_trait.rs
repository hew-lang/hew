mod common;

use common::typecheck;
use hew_types::Ty;

#[test]
fn pid_trait_with_assoc_msg_works() {
    let output = typecheck(
        r"
        type Work {
            id: i32;
        }

        actor Worker {
            let id: i32;
            init() {}
            receive fn handle(msg: Work) {}
        }

        impl ActorMsg for Worker {
            type Msg = Work;
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
        output.errors.is_empty(),
        "Pid associated Msg should type-check cleanly: {:#?}",
        output.errors
    );
}

#[test]
fn local_pid_satisfies_pid_with_actor_msg_assoc() {
    let output = typecheck(
        r"
        type Job {
            n: i32;
        }

        actor Worker {
            let id: i32;
            init() {}
            receive fn run(job: Job) {}
        }

        impl ActorMsg for Worker {
            type Msg = Job;
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
        output.errors.is_empty(),
        "LocalPid<Worker> should satisfy Pid with Msg = Worker ActorMsg::Msg: {:#?}",
        output.errors
    );
}

#[test]
fn local_pid_tell_returns_result_send_error() {
    let (prog, output) = common::parse_and_typecheck_inline(
        r"
        type Job {
            n: i32;
        }

        actor Worker {
            let id: i32;
            init() {}
            receive fn run(job: Job) {}
        }

        impl ActorMsg for Worker {
            type Msg = Job;
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
        type Job {
            n: i32;
        }

        actor Worker {
            let id: i32;
            init() {}
            receive fn run(job: Job) {}
        }

        impl ActorMsg for Worker {
            type Msg = Job;
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
