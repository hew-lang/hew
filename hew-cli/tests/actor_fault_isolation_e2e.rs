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
        Ok(admitted) => if admitted {
            panic("crashing item was admitted")
        },
        Err(_) => panic("crashing push did not settle"),
    }
    match await source.count() {
        Ok(value) => if value != 0 {
            panic("crashing item reached the sink")
        },
        Err(_) => panic("pipeline count did not settle"),
    }
    match await source.push(item(10, "after-crash", false)) {
        Ok(admitted) => if admitted {
            panic("post-crash item was admitted")
        },
        Err(_) => panic("post-crash push did not settle"),
    }
    println("PIPELINE_CRASH_SETTLED");
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
    let mut command = Command::new(binary);
    command.env("HEW_WORKERS", "1");
    run_bounded_command(command, format!("run {}", binary.display()))
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
