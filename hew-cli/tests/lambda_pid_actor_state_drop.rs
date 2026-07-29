//! A `LambdaPid` stored in actor state owns a runtime strong-handle release.
//! Compilation must route that state-drop through the runtime ABI declaration,
//! and shutdown must complete without leaking the actor-state drop body.

mod support;

use support::{assert_success, repo_root, run_bounded_hew_run, tempdir};

#[test]
fn lambda_pid_actor_state_runs_and_tears_down() {
    let dir = tempdir();
    let source = dir.path().join("lambda_pid_actor_state_drop.hew");
    std::fs::write(
        &source,
        r#"
actor Holder {
    let pid: LambdaPid<i64, ()>;

    receive fn ping() -> i64 {
        1
    }
}

fn main() {
    let pid = actor |_n: i64| {};
    let _holder = spawn Holder(pid: pid);
    println("ok");
}
"#,
    )
    .expect("write Hew source");

    let output = run_bounded_hew_run(&source, repo_root());
    assert_success(&output, "LambdaPid actor-state drop must compile and run");
    assert_eq!(String::from_utf8_lossy(&output.stdout).trim(), "ok");
}
