//! Real frontend/codegen regressions for source-owned lifecycle type identity.

mod support;

use std::fs;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen, run_bounded_command};

#[test]
fn root_and_imported_module_members_accept_canonical_lifecycle_payloads() {
    let workspace = tempfile::Builder::new()
        .prefix("lifecycle-members-")
        .tempdir()
        .expect("create lifecycle fixture workspace");
    fs::write(
        workspace.path().join("hew.toml"),
        "[package]\nname = \"lifecyclepkg\"\n",
    )
    .expect("write hew.toml");
    fs::create_dir(workspace.path().join("src")).expect("create src directory");

    let main = workspace.path().join("main.hew");
    fs::write(
        &main,
        r"
import std.failure.{CrashNotification, CrashKind};
import std.link_monitor.{DownNotification, DownReason};
import lifecyclepkg.events;

type RootEnvelope {
    exit: CrashNotification;
    down: DownNotification;
}

enum RootEvent {
    Exit(CrashNotification);
    Kind(CrashKind);
    Down(DownReason);
}

fn main() -> i64 {
    events.code()
}
",
    )
    .expect("write root lifecycle fixture");
    fs::write(
        workspace.path().join("src/events.hew"),
        r"
import std.failure.{CrashNotification};

pub enum PeerEvent {
    Exit(CrashNotification);
}

pub fn code() -> i64 {
    0
}
",
    )
    .expect("write imported lifecycle fixture");

    let output = Command::new(hew_binary())
        .arg("check")
        .arg(&main)
        .env("HEW_STD", repo_root().join("std"))
        .current_dir(workspace.path())
        .output()
        .expect("run hew check");
    assert!(
        output.status.success(),
        "canonical lifecycle types must resolve in root record fields and root/imported \
         enum payloads without panicking in cycle detection:\n{}",
        describe_output(&output)
    );
}

#[test]
fn whole_module_alias_exit_hook_compiles_and_runs_with_scalar_abi() {
    require_codegen();

    let workspace = support::tempdir();
    let source = workspace.path().join("alias_exit.hew");
    fs::write(
        &source,
        r#"
import std.failure as f;

actor Watcher {
    #[on(exit)]
    fn on_peer_exit(note: f.CrashNotification) {
        let _id = note.actor_id;
        let _kind = note.kind;
    }
}

fn main() {
    let _watcher = spawn Watcher;
    println("alias-exit-ok");
}
"#,
    )
    .expect("write aliased exit-hook fixture");

    let mut command = Command::new(hew_binary());
    command.arg("run").arg(&source);
    let output = run_bounded_command(command, "run whole-module alias exit hook");
    assert!(
        output.status.success(),
        "whole-module alias exit hook must compile, link, and run:\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "alias-exit-ok\n",
        "aliased lifecycle annotation must not perturb runtime startup"
    );
}

#[test]
fn down_hook_payload_reads_have_total_mir_decisions() {
    require_codegen();

    let workspace = support::tempdir();
    let source = workspace.path().join("down_payload.hew");
    fs::write(
        &source,
        r#"
import std.link_monitor.{DownNotification, DownReason, DownTarget};

actor Watcher {
    #[on(down)]
    fn on_down(note: DownNotification) {
        let _monitor = note.monitor.value;
        let _target = match note.target {
            DownTarget::Local(slot) => slot,
            DownTarget::Remote(_) => 0,
        };
        let _reason = match note.reason {
            DownReason::Exited => 1,
            DownReason::Crashed(_) => 2,
            DownReason::MonitorLost => 3,
            DownReason::LocalShutdown => 4,
        };
    }
}

fn main() {
    let _watcher = spawn Watcher;
    println("down-payload-ok");
}
"#,
    )
    .expect("write DOWN payload fixture");

    let mut command = Command::new(hew_binary());
    command.arg("run").arg(&source);
    let output = run_bounded_command(command, "run DOWN lifecycle payload reads");
    assert!(
        output.status.success(),
        "every checker-admitted DOWN payload read must reach a concrete MIR decision:\n{}",
        describe_output(&output)
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "down-payload-ok\n");
}
