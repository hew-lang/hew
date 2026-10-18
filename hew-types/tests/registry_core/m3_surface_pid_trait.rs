use crate::common;

use common::typecheck;

#[test]
fn user_pid_trait_does_not_inherit_builtin_serializable_policy() {
    let output = typecheck(
        r"type Work {
    id: i32;
}

type Local {
    id: i32;
}

trait UserPid {
    type Msg;
    fn send(self, msg: Self.Msg) -> i32;
}

impl UserPid for Local {
    type Msg = Work;
    fn send(self, msg: Work) -> i32 {
        self.id + msg.id
    }
}

fn relay<P: UserPid>(pid: P, msg: P.Msg) -> i32 {
    pid.send(msg)
}

fn relay_dyn(pid: dyn UserPid<Msg = Work>, msg: Work) -> i32 {
    pid.send(msg)
}

fn main() {
    let local = Local { id: 1 };
    let _a = relay(local, Work { id: 2 });
    let local_dyn: dyn UserPid<Msg = Work> = Local { id: 3 };
    let _b = relay_dyn(local_dyn, Work { id: 4 });
}
",
    );
    assert!(
        output.errors.is_empty(),
        "an ordinary user trait must not acquire std.builtins.Pid policy: {:#?}",
        output.errors
    );
}

// D489 retired the erased `Pid` trait as user-nameable surface: an actor is
// the type of its handle, so generic dispatch over "any actor handle" has no
// bound left to spell. `<P: Pid>` must fail closed as an unknown trait rather
// than silently reviving the old erasure path or the Serializable fail-closed
// diagnostic the bound used to route through.
#[test]
fn generic_pid_trait_bound_is_no_longer_nameable() {
    let output = typecheck(
        r"type Job {
    n: i32;
}

actor Worker {
    let id: i32;
    init() {}
    receive fn run(job: Job) {}
}

fn takes_pid<P: Pid>(pid: P, msg: P.Msg) -> Result<(), SendError> {
    pid.send(msg)
}

fn main() {
    let worker = spawn Worker { id: 7 };
    let result: Result<(), SendError> = takes_pid(worker, Job { n: 3 });
}
",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.message.contains("unknown trait `Pid`")),
        "a `Pid` bound on a type parameter must fail closed as an unknown trait: {:#?}",
        output.errors
    );
}

#[test]
fn actor_send_without_handler_is_rejected() {
    let output = typecheck(
        r"type Job {
    n: i32;
}

actor Worker {
    let id: i32;
    init() {}
    receive fn run(job: Job) {}
}

fn main() {
    let worker = spawn Worker { id: 7 };
    let result = worker.send(Job { n: 3 });
}
",
    );
    assert!(
        output.errors.iter().any(|error| {
            error.kind == hew_types::error::TypeErrorKind::UndefinedMethod
                && error.message.contains("no `send` handler on `Worker`")
        }),
        "an actor handle without a send handler must be rejected at type-check: {:#?}",
        output.errors
    );
}

#[test]
fn local_pid_remote_conversion_is_not_public() {
    let removed_method = ["to_remote", "_via"].concat();
    let source = r#"
        actor Bot {
            let n: i32;
            init() {}
        }
        fn main() {
            let local = spawn Bot { n: 0 };
            let _remote = local.$METHOD("node-1");
        }
    "#
    .replace("$METHOD", &removed_method);
    let output = typecheck(&source);
    assert!(
        output
            .errors
            .iter()
            .any(|e| matches!(e.kind, hew_types::error::TypeErrorKind::UndefinedMethod)),
        "provenance-free local-to-remote conversion must be undefined: {:#?}",
        output.errors
    );
}
