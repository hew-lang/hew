mod support;

use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use support::{hew_binary, repo_root, require_codegen, run_bounded_command, tempdir};

const CRASHING_CHAT_ROOM: &str = r#"
actor Client {
    receive fn crash() -> i64 {
        panic("client crash")
    }

    receive fn deliver(message: string) {
        println(f"DELIVERED:{message}");
    }
}

actor ChatRoom {
    let first: Client;
    let second: Client;
    let third: Client;

    receive fn broadcast(message: string) {
        first.deliver(message);
        second.deliver(message);
        third.deliver(message);
        println("ROOM_SURVIVED");
    }

    receive fn fence() -> i64 {
        1
    }
}

fn main() {
    let crashed = spawn Client;
    let second = spawn Client;
    let third = spawn Client;

    match await crashed.crash() {
        .Ok(_) => println("CRASH_UNEXPECTEDLY_REPLIED"),
        .Err(_) => println("CLIENT_CRASHED"),
    }

    let room = spawn ChatRoom(first: crashed, second: second, third: third);
    room.broadcast("after-crash");
    match await room.fence() {
        .Ok(_) => println("ROOM_FENCE_REPLIED"),
        .Err(_) => println("ROOM_DIED"),
    }
}
"#;

const CLEAN_CHAT_ROOM: &str = r#"
actor Client {
    receive fn deliver(message: string) {
        println(f"DELIVERED:{message}");
    }
}

actor ChatRoom {
    let first: Client;
    let second: Client;
    let third: Client;

    receive fn broadcast(message: string) {
        first.deliver(message);
        second.deliver(message);
        third.deliver(message);
        println("ROOM_SURVIVED");
    }

    receive fn fence() -> i64 {
        1
    }
}

fn main() {
    let first = spawn Client;
    let second = spawn Client;
    let third = spawn Client;
    let room = spawn ChatRoom(first: first, second: second, third: third);
    room.broadcast("clean");
    match await room.fence() {
        .Ok(_) => println("ROOM_FENCE_REPLIED"),
        .Err(_) => println("ROOM_DIED"),
    }
}
"#;

const CRASHING_PIPELINE: &str = r#"
import std.pipeline;

fn item(value: i64, label: string, crash_stage: bool) -> pipeline.PipelineItemI64 {
    PipelineItemI64 { value: value, label: label, crash_stage: crash_stage }
}

fn main() {
    let source = pipeline.run(pipeline.from(1));
    match await source.push(item(9, "crash-owned", true)) {
        .Ok(admitted) => if admitted {
            panic("crashing item was admitted")
        },
        .Err(_) => panic("crashing push did not settle"),
    }
    match await source.count() {
        .Ok(value) => if value != 0 {
            panic("crashing item reached the sink")
        },
        .Err(_) => panic("pipeline count did not settle"),
    }
    match await source.push(item(10, "after-crash", false)) {
        .Ok(admitted) => if admitted {
            panic("post-crash item was admitted")
        },
        .Err(_) => panic("post-crash push did not settle"),
    }
    println("PIPELINE_CRASH_SETTLED");
}
"#;

/// A supervisor program that ALSO spawns an unsupervised actor which crashes.
/// The supervisor recovers what it supervises and nothing else, so the loner's
/// unrecovered fault owns the exit status. This is the shape that exited 0
/// while the crash flag was only read on the implicit-drain shutdown path: a
/// program containing a supervisor takes the immediate `hew_sched_shutdown`
/// epilogue instead, and never consulted the flag.
const SUPERVISOR_PLUS_UNSUPERVISED_CRASHER: &str = r#"
actor Worker {
    let id: i64;

    receive fn work() {
        println(f"WORKED:{id}");
    }
}

actor Loner {
    receive fn boom() -> i64 {
        panic("unsupervised loner crash")
    }
}

supervisor Pool {
    strategy: one_for_one;
    intensity: 5 within 60s;

    child w1: Worker(id: 1);
}

fn main() {
    let sup = spawn Pool;
    sleep(30ms);
    let w1 = sup.w1;
    w1.work();
    sleep(20ms);

    let loner = spawn Loner;
    match await loner.boom() {
        .Ok(_) => println("LONER_REPLIED"),
        .Err(_) => println("LONER_CRASHED"),
    }
    println("MAIN_DONE");
}
"#;

/// A supervisor that restarts its OWN child. The fault is handled by the
/// authority that owns it, so the run is successful — the one thing that keeps
/// a crashed actor out of the exit status.
const SUPERVISOR_RESTARTS_ITS_CHILD: &str = r#"
actor Flaky {
    let id: i64;

    receive fn work() {
        println(f"WORKED:{id}");
    }

    receive fn boom() {
        panic("supervised child crash")
    }
}

supervisor Pool {
    strategy: one_for_one;
    intensity: 5 within 60s;

    child f1: Flaky(id: 1);
}

fn main() {
    let sup = spawn Pool;
    sleep(30ms);
    var f1 = sup.f1;
    f1.work();
    sleep(20ms);
    f1.boom();
    sleep(500ms);
    f1 = sup.f1;
    f1.work();
    sleep(50ms);
    println("MAIN_DONE");
}
"#;

/// A supervisor whose restart budget is exhausted: the child keeps failing, the
/// supervisor gives up, and it has no parent to escalate to. The fault reached
/// the top of the supervision tree unrecovered, so it owns the exit status.
const SUPERVISOR_EXHAUSTS_ITS_BUDGET: &str = r#"
actor Fragile {
    receive fn boom() {
        panic("persistent child failure")
    }
}

supervisor Pool {
    strategy: one_for_one;
    intensity: 1 within 60s;

    child f1: Fragile;
}

fn main() {
    let sup = spawn Pool;
    sleep(30ms);
    var f1 = sup.f1;
    f1.boom();
    sleep(400ms);
    f1 = sup.f1;
    f1.boom();
    sleep(600ms);
    println("MAIN_DONE");
}
"#;

/// Crash IN the supervisor: the child's `#[on(crash)]` hook runs on the
/// supervisor's own actor, so panicking there makes the supervisor terminal. A
/// root supervisor has no supervisor of its own, so that crash is unrecovered.
/// The independent probe must still answer — the fault is reported, not
/// cascaded.
const CRASH_IN_SUPERVISOR_ITSELF: &str = r#"
import std.failure;

actor Worker {
    #[on(crash)]
    fn on_crash(info: CrashInfo) -> CrashAction {
        panic("crash hook itself failed")
    }

    receive fn boom() {
        panic("child crash that fires the hook")
    }
}

actor Probe {
    receive fn ping() -> i64 {
        7
    }
}

supervisor App {
    strategy: one_for_one;
    intensity: 5 within 60s;

    child w: Worker;
}

fn main() {
    let sup = spawn App;
    let probe = spawn Probe;
    let w = sup.w;
    w.boom();
    sleep(300ms);
    match await probe.ping() {
        .Ok(v) => println(f"PROBE:{v}"),
        .Err(_) => println("PROBE_DEAD"),
    }
    println("MAIN_DONE");
}
"#;

/// Crash raised AFTER shutdown began: `main` returns while the straggler's
/// handler is still queued, so the crash lands inside the exit drain. Timing
/// does not change the rule — the fault is unrecovered and the process fails.
const CRASH_AFTER_SHUTDOWN_BEGAN: &str = r#"
actor Straggler {
    receive fn late() {
        sleep(120ms);
        panic("crash raised while the exit drain was running");
    }
}

fn main() {
    let s = spawn Straggler;
    s.late();
    println("MAIN_RETURNED_BEFORE_CRASH");
}
"#;

/// Crash DURING suspend: the waiter parks on an `ask` and crashes on the resume
/// edge, so the fault is raised from a resumed coroutine frame rather than a
/// first dispatch. The probe must still answer.
const CRASH_ON_SUSPEND_RESUME: &str = r#"
actor Slow {
    receive fn fetch() -> i64 {
        sleep(80ms);
        42
    }
}

actor Waiter {
    let slow: Slow;

    receive fn drive() -> i64 {
        match await slow.fetch() {
            .Ok(v) => panic(f"crash after resuming from suspend with {v}"),
            .Err(_) => panic("crash after a failed suspend"),
        }
    }
}

actor Probe {
    receive fn ping() -> i64 {
        7
    }
}

fn main() {
    let slow = spawn Slow;
    let waiter = spawn Waiter(slow: slow);
    let probe = spawn Probe;
    match await waiter.drive() {
        .Ok(_) => println("WAITER_REPLIED"),
        .Err(_) => println("WAITER_CRASHED"),
    }
    match await probe.ping() {
        .Ok(v) => println(f"PROBE:{v}"),
        .Err(_) => println("PROBE_DEAD"),
    }
    println("MAIN_DONE");
}
"#;

/// A `temporary` child that CRASHES. Policy declines the restart, so no
/// recovery happens: declining is not recovering, and the crash owns the exit
/// status. The supervisor itself keeps running — the fault is reported, not
/// cascaded.
const TEMPORARY_CHILD_CRASH: &str = r#"
actor OneShot {
    receive fn boom() {
        panic("temporary child crash")
    }
}

actor Probe {
    receive fn ping() -> i64 {
        7
    }
}

supervisor Pool {
    strategy: one_for_one;
    intensity: 5 within 60s;

    child t1: OneShot restart: temporary;
}

fn main() {
    let sup = spawn Pool;
    let probe = spawn Probe;
    sleep(30ms);
    let t1 = sup.t1;
    t1.boom();
    sleep(300ms);
    match await probe.ping() {
        .Ok(v) => println(f"PROBE:{v}"),
        .Err(_) => println("PROBE_DEAD"),
    }
    println("MAIN_DONE");
}
"#;

/// An `#[on(crash)]` hook answering `Escalate` on a ROOT supervisor. There is
/// no parent to escalate to, so the fault reached the top of the supervision
/// tree with no authority left.
const ESCALATE_AT_ROOT: &str = r#"
import std.failure;

actor Worker {
    #[on(crash)]
    fn on_crash(info: CrashInfo) -> CrashAction {
        CrashAction.Escalate
    }

    receive fn boom() {
        panic("crash escalated past the root")
    }
}

actor Probe {
    receive fn ping() -> i64 {
        7
    }
}

supervisor App {
    strategy: one_for_one;
    intensity: 5 within 60s;

    child w: Worker;
}

fn main() {
    let sup = spawn App;
    let probe = spawn Probe;
    let w = sup.w;
    w.boom();
    sleep(300ms);
    match await probe.ping() {
        .Ok(v) => println(f"PROBE:{v}"),
        .Err(_) => println("PROBE_DEAD"),
    }
    println("MAIN_DONE");
}
"#;

/// A supervised crash queued as `main` returns. The supervisor program takes
/// the immediate shutdown path, so the crash and its supervisor's ruling both
/// race the worker join. The exit status must be the ruling's — a restart, so
/// success — not whichever side of the join the scheduler happened to land on.
const SUPERVISED_CRASH_RACING_SHUTDOWN: &str = r#"
actor Flaky {
    receive fn boom() {
        panic("supervised crash racing the exit path")
    }
}

supervisor Pool {
    strategy: one_for_one;
    intensity: 5 within 60s;

    child f1: Flaky;
}

fn main() {
    let sup = spawn Pool;
    sleep(30ms);
    let f1 = sup.f1;
    f1.boom();
    println("MAIN_RETURNS_IMMEDIATELY");
}
"#;

/// A non-zero code the program returned, alongside an unrecovered fault. The
/// chosen code is already a failure and says more than `1`, so it survives.
const USER_EXIT_CODE_WITH_FAULT: &str = r#"
actor Loner {
    receive fn boom() -> i64 {
        panic("unsupervised crash under a user exit code")
    }
}

fn main() -> i64 {
    let loner = spawn Loner;
    match await loner.boom() {
        .Ok(_) => println("LONER_REPLIED"),
        .Err(_) => println("LONER_CRASHED"),
    }
    7
}
"#;

/// An explicit `exit(0)` after an unrecovered fault. The `exit` builtin is a
/// termination path like any other, so it consults the same authority: a zero
/// cannot report success over a crashed actor nobody recovered.
const EXPLICIT_EXIT_ZERO_WITH_FAULT: &str = r#"
actor Loner {
    receive fn boom() -> i64 {
        panic("unsupervised crash before an explicit exit(0)")
    }
}

fn main() {
    let loner = spawn Loner;
    match await loner.boom() {
        .Ok(_) => println("LONER_REPLIED"),
        .Err(_) => println("LONER_CRASHED"),
    }
    exit(0);
}
"#;

/// The same explicit `exit`, with a code the program chose. Nothing overwrites
/// it, fault or not.
const EXPLICIT_EXIT_CODE_WITH_FAULT: &str = r#"
actor Loner {
    receive fn boom() -> i64 {
        panic("unsupervised crash before an explicit exit(3)")
    }
}

fn main() {
    let loner = spawn Loner;
    match await loner.boom() {
        .Ok(_) => println("LONER_REPLIED"),
        .Err(_) => println("LONER_CRASHED"),
    }
    exit(3);
}
"#;

/// Nested authority transfer: the inner supervisor exhausts its budget and
/// ESCALATES to the outer one, which restarts the whole subtree. The escalated
/// record is provisional until the outer supervisor rules, and its restart
/// CLEARS it — a tree that recovered a subtree did what a tree is for.
const NESTED_ESCALATION_RECOVERED_BY_PARENT: &str = r#"
actor Flaky {
    receive fn work() {
        println("WORKED");
    }

    receive fn boom() {
        panic("inner child crash")
    }
}

supervisor Inner {
    strategy: one_for_one;
    intensity: 1 within 60s;

    child f1: Flaky;
}

supervisor Outer {
    strategy: one_for_one;
    intensity: 5 within 60s;

    child inner: Inner;
}

fn main() {
    let outer = spawn Outer;
    sleep(80ms);
    var inner = outer.inner;
    var f1 = inner.f1;
    f1.work();
    sleep(40ms);

    f1.boom();
    sleep(400ms);
    inner = outer.inner;
    f1 = inner.f1;
    f1.boom();
    sleep(800ms);

    inner = outer.inner;
    f1 = inner.f1;
    f1.work();
    sleep(100ms);
    println("MAIN_DONE");
}
"#;

/// Two unrelated supervisors: one recovers its child, the other exhausts its
/// budget. Settling the recovered record must not settle the other one — a
/// ruling settles exactly the record it is about.
const TWO_SUPERVISORS_ONE_HANDLED_ONE_NOT: &str = r#"
actor Flaky {
    let id: i64;

    receive fn work() {
        println(f"WORKED:{id}");
    }

    receive fn boom() {
        panic("child crash")
    }
}

supervisor Recovering {
    strategy: one_for_one;
    intensity: 5 within 60s;

    child r1: Flaky(id: 1);
}

supervisor GivingUp {
    strategy: one_for_one;
    intensity: 1 within 60s;

    child g1: Flaky(id: 2);
}

fn main() {
    let good = spawn Recovering;
    let bad = spawn GivingUp;
    sleep(50ms);

    var r1 = good.r1;
    r1.boom();
    sleep(400ms);
    r1 = good.r1;
    r1.work();
    sleep(40ms);

    var g1 = bad.g1;
    g1.boom();
    sleep(400ms);
    g1 = bad.g1;
    g1.boom();
    sleep(600ms);
    println("MAIN_DONE");
}
"#;

/// A SUSPENDING `main`. Its body is lowered as a coroutine, so its return does
/// not take the ordinary path — the exit-status consult has to be on the shape
/// `main` actually compiles to, not the one it usually compiles to.
const SUSPENDING_MAIN_WITH_FAULT: &str = r#"
actor Slow {
    receive fn fetch() -> i64 {
        sleep(60ms);
        11
    }
}

actor Loner {
    receive fn boom() -> i64 {
        panic("unsupervised crash in a suspending main")
    }
}

fn main() {
    let slow = spawn Slow;
    let loner = spawn Loner;
    match await slow.fetch() {
        .Ok(v) => println(f"FETCHED:{v}"),
        .Err(_) => println("FETCH_FAILED"),
    }
    match await loner.boom() {
        .Ok(_) => println("LONER_REPLIED"),
        .Err(_) => println("LONER_CRASHED"),
    }
    println("MAIN_DONE");
}
"#;

/// A `main` that drives a generator to completion and also faults an actor.
/// The generator drive introduces its own suspend/resume machinery around the
/// return; the fault must still reach the exit status.
const GENERATOR_MAIN_WITH_FAULT: &str = r#"
gen fn counter(limit: i64) -> i64 {
    var i: i64 = 0;
    while i < limit {
        yield i;
        i = i + 1;
    }
}

actor Loner {
    receive fn boom() -> i64 {
        panic("unsupervised crash beside a generator drive")
    }
}

fn main() {
    var total: i64 = 0;
    for value in counter(4) {
        total = total + value;
    }
    println(f"TOTAL:{total}");

    let loner = spawn Loner;
    match await loner.boom() {
        .Ok(_) => println("LONER_REPLIED"),
        .Err(_) => println("LONER_CRASHED"),
    }
    println("MAIN_DONE");
}
"#;

fn compile_fixture(source: &str, dir: &Path, name: &str) -> PathBuf {
    let source_path = dir.join(format!("{name}.hew"));
    std::fs::write(&source_path, source).expect("write chat-room fixture");

    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--emit-dir",
            dir.to_str().expect("fixture directory is utf-8"),
            source_path.to_str().expect("fixture path is utf-8"),
        ])
        .current_dir(repo_root())
        .output()
        .expect("compile chat-room fixture");

    assert!(
        output.status.success(),
        "compile {name} failed:\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    let binary = stdout
        .lines()
        .find_map(|line| line.strip_prefix("native: "))
        .unwrap_or_else(|| panic!("compile {name} did not report a native binary:\n{stdout}"));
    PathBuf::from(binary)
}

fn run_fixture(binary: &Path) -> Output {
    run_fixture_with_workers(binary, "1")
}

/// Worker-pool settings every fault fixture is replayed under.
///
/// `1` pins the single-worker interleaving (crash and sender on the same
/// thread); `4` puts the crashing actor, its supervisor and the sender on
/// different workers, which is where the terminal-transition races live. A
/// fault rule that only holds on one of these is not a rule.
const WORKER_POOLS: [&str; 2] = ["1", "4"];

fn run_fixture_with_workers(binary: &Path, workers: &str) -> Output {
    let mut command = Command::new(binary);
    command.env("HEW_WORKERS", workers);
    run_bounded_command(
        command,
        format!("run {} with HEW_WORKERS={workers}", binary.display()),
    )
}

/// Compile `source` once and replay it under every pool in [`WORKER_POOLS`],
/// asserting the exit status and required stdout/stderr fragments each time.
fn assert_fixture_under_every_worker_pool(
    source: &str,
    name: &str,
    expected_status: i32,
    stdout_contains: &[&str],
    stderr_contains: &[&str],
) {
    assert_fixture_counts_under_every_worker_pool(
        source,
        name,
        expected_status,
        stdout_contains,
        &[],
        stderr_contains,
    );
}

/// As [`assert_fixture_under_every_worker_pool`], plus minimum OCCURRENCE
/// counts on stdout.
///
/// A bare `contains` cannot distinguish "the child worked" from "the child
/// worked again after its supervisor restarted it" when both print the same
/// line — the second occurrence is the whole claim, and only a count sees it.
fn assert_fixture_counts_under_every_worker_pool(
    source: &str,
    name: &str,
    expected_status: i32,
    stdout_contains: &[&str],
    stdout_min_counts: &[(&str, usize)],
    stderr_contains: &[&str],
) {
    let dir = tempdir();
    let binary = compile_fixture(source, dir.path(), name);

    for workers in WORKER_POOLS {
        let output = run_fixture_with_workers(&binary, workers);
        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert_eq!(
            output.status.code(),
            Some(expected_status),
            "{name} under HEW_WORKERS={workers} must exit {expected_status}:\nstdout:\n{stdout}\nstderr:\n{stderr}",
        );
        for fragment in stdout_contains {
            assert!(
                stdout.contains(fragment),
                "{name} under HEW_WORKERS={workers} must print {fragment:?}:\nstdout:\n{stdout}\nstderr:\n{stderr}",
            );
        }
        for (fragment, minimum) in stdout_min_counts {
            let seen = stdout.matches(fragment).count();
            assert!(
                seen >= *minimum,
                "{name} under HEW_WORKERS={workers} must print {fragment:?} at least {minimum} \
                 times, saw {seen}:\nstdout:\n{stdout}\nstderr:\n{stderr}",
            );
        }
        for fragment in stderr_contains {
            assert!(
                stderr.contains(fragment),
                "{name} under HEW_WORKERS={workers} must report {fragment:?} on stderr:\nstdout:\n{stdout}\nstderr:\n{stderr}",
            );
        }
    }
}

#[test]
fn actor_crash_fails_process_without_cascading_to_chat_room() {
    require_codegen();

    let dir = tempdir();
    let crashing = compile_fixture(CRASHING_CHAT_ROOM, dir.path(), "crashing_chat_room");
    let crash_output = run_fixture(&crashing);
    let crash_stdout = String::from_utf8_lossy(&crash_output.stdout);
    let crash_stderr = String::from_utf8_lossy(&crash_output.stderr);

    assert_eq!(
        crash_output.status.code(),
        Some(1),
        "an unsupervised actor panic must fail the process:\nstdout:\n{crash_stdout}\nstderr:\n{crash_stderr}",
    );
    assert!(
        crash_stdout.contains("ROOM_SURVIVED")
            && crash_stdout.contains("ROOM_FENCE_REPLIED")
            && crash_stdout.contains("DELIVERED:after-crash"),
        "the room must survive broadcasting past the crashed client:\nstdout:\n{crash_stdout}\nstderr:\n{crash_stderr}",
    );
    assert!(
        crash_stderr.contains("client crash"),
        "the client crash must remain observable:\nstdout:\n{crash_stdout}\nstderr:\n{crash_stderr}",
    );

    let clean = compile_fixture(CLEAN_CHAT_ROOM, dir.path(), "clean_chat_room");
    let clean_output = run_fixture(&clean);
    let clean_stdout = String::from_utf8_lossy(&clean_output.stdout);
    let clean_stderr = String::from_utf8_lossy(&clean_output.stderr);

    assert_eq!(
        clean_output.status.code(),
        Some(0),
        "a clean chat-room run must remain successful:\nstdout:\n{clean_stdout}\nstderr:\n{clean_stderr}",
    );
    assert!(
        clean_stdout.contains("ROOM_SURVIVED") && clean_stdout.contains("ROOM_FENCE_REPLIED"),
        "the clean room must complete its broadcast:\nstdout:\n{clean_stdout}\nstderr:\n{clean_stderr}",
    );
}

#[test]
fn pipeline_actor_crash_fails_process_after_settling_pending_work() {
    require_codegen();

    let dir = tempdir();
    let binary = compile_fixture(CRASHING_PIPELINE, dir.path(), "crashing_pipeline");
    let output = run_fixture(&binary);
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert_eq!(
        output.status.code(),
        Some(1),
        "the pipeline actor crash must fail the process:\nstdout:\n{stdout}\nstderr:\n{stderr}",
    );
    assert!(
        stdout.contains("PIPELINE_CRASH_SETTLED"),
        "the pipeline must reject pending work before reporting its crash:\nstdout:\n{stdout}\nstderr:\n{stderr}",
    );
    assert!(
        stderr.contains("pipeline S1 stage crash"),
        "the pipeline stage crash must remain observable:\nstdout:\n{stdout}\nstderr:\n{stderr}",
    );
}

/// A supervisor recovers what it supervises and nothing else: an unrelated
/// unsupervised crash still fails the process.
#[test]
fn supervisor_program_reports_an_unrelated_unsupervised_crash() {
    require_codegen();
    assert_fixture_under_every_worker_pool(
        SUPERVISOR_PLUS_UNSUPERVISED_CRASHER,
        "supervisor_plus_unsupervised_crasher",
        1,
        &["WORKED:1", "LONER_CRASHED", "MAIN_DONE"],
        &["unsupervised loner crash"],
    );
}

/// A supervisor restarting its own child HANDLES the fault, and a handled fault
/// leaves the exit status successful.
#[test]
fn supervisor_restarting_its_own_child_keeps_the_run_successful() {
    require_codegen();
    // The fixture works the child once BEFORE the crash and once AFTER, so a
    // bare `contains("WORKED:1")` passes on the pre-crash line alone and says
    // nothing about the restart. Two occurrences is the claim: the child came
    // back and served again.
    assert_fixture_counts_under_every_worker_pool(
        SUPERVISOR_RESTARTS_ITS_CHILD,
        "supervisor_restarts_its_child",
        0,
        &["MAIN_DONE"],
        &[("WORKED:1", 2)],
        &["supervised child crash"],
    );
}

/// A root supervisor that exhausts its restart budget has given up: the fault
/// reached the top of the tree unrecovered and owns the exit status.
#[test]
fn root_supervisor_exhausting_its_budget_fails_the_process() {
    require_codegen();
    assert_fixture_under_every_worker_pool(
        SUPERVISOR_EXHAUSTS_ITS_BUDGET,
        "supervisor_exhausts_its_budget",
        1,
        &["MAIN_DONE"],
        &["persistent child failure"],
    );
}

/// A crash in the supervisor's own actor is unrecovered (a root supervisor has
/// no supervisor), and must be reported without cascading to unrelated actors.
#[test]
fn crash_in_the_supervisor_itself_fails_the_process_without_cascading() {
    require_codegen();
    assert_fixture_under_every_worker_pool(
        CRASH_IN_SUPERVISOR_ITSELF,
        "crash_in_supervisor_itself",
        1,
        &["PROBE:7", "MAIN_DONE"],
        &["crash hook itself failed"],
    );
}

/// A fault raised after shutdown began is still a fault: the exit-status
/// authority is read after the drain each shutdown path performs.
#[test]
fn crash_after_shutdown_began_fails_the_process() {
    require_codegen();
    assert_fixture_under_every_worker_pool(
        CRASH_AFTER_SHUTDOWN_BEGAN,
        "crash_after_shutdown_began",
        1,
        &["MAIN_RETURNED_BEFORE_CRASH"],
        &["crash raised while the exit drain was running"],
    );
}

/// A crash on the resume edge of a suspended handler is reported like any other
/// unsupervised fault, and does not take the scheduler with it.
#[test]
fn crash_on_suspend_resume_fails_the_process_without_cascading() {
    require_codegen();
    assert_fixture_under_every_worker_pool(
        CRASH_ON_SUSPEND_RESUME,
        "crash_on_suspend_resume",
        1,
        &["WAITER_CRASHED", "PROBE:7", "MAIN_DONE"],
        &["crash after resuming from suspend"],
    );
}

/// A `temporary` child's crash is declined, not recovered: the exit status
/// reports it while the supervisor keeps serving.
#[test]
fn temporary_child_crash_is_not_a_recovery() {
    require_codegen();
    assert_fixture_under_every_worker_pool(
        TEMPORARY_CHILD_CRASH,
        "temporary_child_crash",
        1,
        &["PROBE:7", "MAIN_DONE"],
        &["temporary child crash"],
    );
}

/// `Escalate` at a root supervisor has nowhere to route: the fault reached the
/// top of the tree unrecovered.
#[test]
fn crash_escalated_past_the_root_supervisor_fails_the_process() {
    require_codegen();
    assert_fixture_under_every_worker_pool(
        ESCALATE_AT_ROOT,
        "escalate_at_root",
        1,
        &["PROBE:7", "MAIN_DONE"],
        &["crash escalated past the root"],
    );
}

/// A supervised crash queued as `main` returns is still ruled on: shutdown
/// quiesces the queued work AND the supervisor decision it raises before
/// joining workers, so the restart decides the status.
///
/// The RACE itself is pinned deterministically in `hew-runtime`
/// (`scheduler::tests::a_popped_actor_awaiting_activation_is_not_idle`), which
/// parks a real worker in the pop-to-activation window with the queue-handoff
/// hook. This is the end-to-end shape over that fix, not the probe for it —
/// repeating a racy run to try to catch a race is what that unit test replaces.
#[test]
fn supervised_crash_racing_shutdown_is_still_ruled_on() {
    require_codegen();
    assert_fixture_under_every_worker_pool(
        SUPERVISED_CRASH_RACING_SHUTDOWN,
        "supervised_crash_racing_shutdown",
        0,
        &["MAIN_RETURNS_IMMEDIATELY"],
        &["supervised crash racing the exit path"],
    );
}

/// A non-zero code the program returned survives the fault report.
#[test]
fn returned_exit_code_survives_an_unrecovered_fault() {
    require_codegen();
    assert_fixture_under_every_worker_pool(
        USER_EXIT_CODE_WITH_FAULT,
        "user_exit_code_with_fault",
        7,
        &["LONER_CRASHED"],
        &["unsupervised crash under a user exit code"],
    );
}

/// `exit(0)` cannot report success over an unrecovered fault.
#[test]
fn explicit_exit_zero_does_not_mask_an_unrecovered_fault() {
    require_codegen();
    assert_fixture_under_every_worker_pool(
        EXPLICIT_EXIT_ZERO_WITH_FAULT,
        "explicit_exit_zero_with_fault",
        1,
        &["LONER_CRASHED"],
        &["unsupervised crash before an explicit exit(0)"],
    );
}

/// `exit(code)` keeps the code the program chose.
#[test]
fn explicit_exit_code_survives_an_unrecovered_fault() {
    require_codegen();
    assert_fixture_under_every_worker_pool(
        EXPLICIT_EXIT_CODE_WITH_FAULT,
        "explicit_exit_code_with_fault",
        3,
        &["LONER_CRASHED"],
        &["unsupervised crash before an explicit exit(3)"],
    );
}

/// An escalated record is provisional: the parent's restart of the subtree
/// clears it. Concluding "handled" from the escalation SEND, or "unrecovered"
/// from the child's own give-up, both get this wrong.
#[test]
fn nested_escalation_recovered_by_the_parent_keeps_the_run_successful() {
    require_codegen();
    assert_fixture_counts_under_every_worker_pool(
        NESTED_ESCALATION_RECOVERED_BY_PARENT,
        "nested_escalation_recovered_by_parent",
        0,
        &["MAIN_DONE"],
        &[("WORKED", 2)],
        &["inner child crash"],
    );
}

/// A ruling settles exactly one record: the recovered supervisor's restart must
/// not clear the record of the one that gave up.
#[test]
fn one_recovered_supervisor_does_not_clear_another_that_gave_up() {
    require_codegen();
    assert_fixture_under_every_worker_pool(
        TWO_SUPERVISORS_ONE_HANDLED_ONE_NOT,
        "two_supervisors_one_handled_one_not",
        1,
        &["WORKED:1", "MAIN_DONE"],
        &["child crash"],
    );
}

/// A suspending `main` compiles to a coroutine; the exit-status consult must be
/// on that shape too.
#[test]
fn suspending_main_reports_an_unrecovered_fault() {
    require_codegen();
    assert_fixture_under_every_worker_pool(
        SUSPENDING_MAIN_WITH_FAULT,
        "suspending_main_with_fault",
        1,
        &["FETCHED:11", "LONER_CRASHED", "MAIN_DONE"],
        &["unsupervised crash in a suspending main"],
    );
}

/// A `main` that drives a generator likewise.
#[test]
fn generator_driving_main_reports_an_unrecovered_fault() {
    require_codegen();
    assert_fixture_under_every_worker_pool(
        GENERATOR_MAIN_WITH_FAULT,
        "generator_main_with_fault",
        1,
        &["TOTAL:6", "LONER_CRASHED", "MAIN_DONE"],
        &["unsupervised crash beside a generator drive"],
    );
}
