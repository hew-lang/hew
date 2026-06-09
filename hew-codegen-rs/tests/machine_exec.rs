//! End-to-end machine execution fixtures for `m.step(ev)` and `m.state_name()`.
//!
//! The CLI currently emits textual LLVM IR as part of `hew compile`; there is
//! no separate `--emit-llvm` flag. These tests invoke that public compile path,
//! read the emitted `.ll`, and then run the same fixtures through `hew run`.

use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::OnceLock;

struct MachineFixture {
    stem: &'static str,
    machine: &'static str,
    states: usize,
}

const FIXTURES: &[MachineFixture] = &[
    MachineFixture {
        stem: "run_traffic_light",
        machine: "TrafficLight",
        states: 3,
    },
    MachineFixture {
        stem: "run_tcp_handshake",
        machine: "TcpHandshake",
        states: 3,
    },
];

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-codegen-rs has a workspace parent")
        .to_path_buf()
}

fn target_dir(repo: &Path) -> PathBuf {
    std::env::var_os("CARGO_TARGET_DIR").map_or_else(
        || repo.join("target"),
        |dir| {
            let path = PathBuf::from(dir);
            if path.is_absolute() {
                path
            } else {
                repo.join(path)
            }
        },
    )
}

fn hew_bin(repo: &Path) -> PathBuf {
    target_dir(repo).join("debug").join("hew")
}

fn hew_command(repo: &Path) -> Command {
    let bin = hew_bin(repo);
    if bin.exists() {
        return Command::new(bin);
    }

    let cargo = std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into());
    let mut command = Command::new(cargo);
    command
        .current_dir(repo)
        .args(["run", "--quiet", "-p", "hew-cli", "--bin", "hew", "--"]);
    command
}

fn ensure_hew_runtime_lib(repo: &Path) {
    static BUILT: OnceLock<()> = OnceLock::new();
    BUILT.get_or_init(|| {
        let lib = target_dir(repo).join("debug").join("libhew.a");
        if lib.exists() {
            return;
        }
        let cargo = std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into());
        let status = Command::new(cargo)
            .current_dir(repo)
            .args(["build", "--quiet", "-p", "hew-lib"])
            .status()
            .expect("spawn cargo build -p hew-lib");
        assert!(
            status.success(),
            "cargo build -p hew-lib failed: {status:?}"
        );
    });
}

fn fixture_path(repo: &Path, stem: &str) -> PathBuf {
    repo.join("examples")
        .join("machine")
        .join(format!("{stem}.hew"))
}

fn compile_fixture(repo: &Path, fixture: &MachineFixture) -> String {
    ensure_hew_runtime_lib(repo);
    let out_dir = std::env::temp_dir().join(format!(
        "hew-machine-exec-{}-{}",
        fixture.stem,
        std::process::id()
    ));
    let _ = std::fs::remove_dir_all(&out_dir);
    std::fs::create_dir_all(&out_dir).expect("create compile output dir");

    let output = hew_command(repo)
        .arg("compile")
        .arg(fixture_path(repo, fixture.stem))
        .arg("--emit-dir")
        .arg(&out_dir)
        .output()
        .expect("spawn hew compile");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        output.status.success(),
        "hew compile {} exited non-zero (status={:?}); stderr:\n{}",
        fixture.stem,
        output.status,
        stderr
    );
    assert!(
        !stderr.contains("cutover")
            && !stderr.contains("E_NOT_YET_IMPLEMENTED")
            && !stderr.contains("E_HIR")
            && !stderr.contains("E_MIR"),
        "machine fixture {} must compile without cutover/HIR/MIR diagnostics; stderr:\n{}",
        fixture.stem,
        stderr
    );

    std::fs::read_to_string(out_dir.join(format!("{}.ll", fixture.stem))).unwrap_or_else(|e| {
        panic!(
            "read emitted LLVM IR for {} from {}: {e}",
            fixture.stem,
            out_dir.display()
        )
    })
}

fn step_function_body<'a>(ir: &'a str, machine: &str) -> &'a str {
    let marker = format!("define %{} @{}__step(", machine, machine);
    let start = ir
        .find(&marker)
        .or_else(|| ir.find(&format!("@{}__step(", machine)))
        .unwrap_or_else(|| panic!("missing step helper `{machine}__step` in IR:\n{ir}"));
    let tail = &ir[start..];
    let end = tail.find("\n}\n").unwrap_or(tail.len());
    &tail[..end]
}

fn main_function_body(ir: &str) -> &str {
    let start = ir
        .find("define i8 @main(")
        .unwrap_or_else(|| panic!("missing user main function in IR:\n{ir}"));
    let tail = &ir[start..];
    let end = tail.find("\n}\n").unwrap_or(tail.len());
    &tail[..end]
}

#[test]
fn run_machine_fixtures_compile_to_step_dispatch_and_state_table() {
    let repo = repo_root();
    ensure_hew_runtime_lib(&repo);
    for fixture in FIXTURES {
        let ir = compile_fixture(&repo, fixture);
        let step_body = step_function_body(&ir, fixture.machine);
        let main_body = main_function_body(&ir);

        assert!(
            ir.contains(&format!("@{}__step(", fixture.machine)),
            "{} LLVM must contain the synthesised step helper",
            fixture.stem
        );
        assert!(
            step_body.contains("switch") || step_body.contains("icmp eq"),
            "{} step helper must contain dispatch over state/event tags:\n{}",
            fixture.stem,
            step_body
        );
        assert!(
            step_body.contains("unreachable"),
            "{} step dispatch fallthrough must lower to LLVM unreachable:\n{}",
            fixture.stem,
            step_body
        );
        assert!(
            !step_body.contains("@hew_trap_with_code(i32 207)"),
            "{} step dispatch fallthrough must not lower to MachineDispatchUnreachable trap:\n{}",
            fixture.stem,
            step_body
        );
        assert!(
            main_body.contains(&format!(
                "call %{} @{}__step(",
                fixture.machine, fixture.machine
            )),
            "{} call site must call the internal step helper and not expose its result:\n{}",
            fixture.stem,
            main_body
        );
        assert!(
            main_body.contains("store %")
                && main_body.contains("%call_result")
                && main_body.contains(&format!("store %{} %move_load", fixture.machine))
                && main_body.contains("ptr %local_"),
            "{} call site must store the step helper result back through MIR Move:\n{}",
            fixture.stem,
            main_body
        );

        let table = format!(
            "@__hew_state_name_table__{} = private constant [{} x ptr]",
            fixture.machine, fixture.states
        );
        assert!(
            ir.contains(&table),
            "{} LLVM must contain the static state_name table `{}`:\n{}",
            fixture.stem,
            table,
            ir
        );
    }
}

#[test]
fn run_machine_fixtures_execute_with_expected_stdout() {
    let repo = repo_root();
    for fixture in FIXTURES {
        let path = fixture_path(&repo, fixture.stem);
        let expected_path = path.with_extension("expected");
        let expected = std::fs::read_to_string(&expected_path)
            .unwrap_or_else(|e| panic!("read {}: {e}", expected_path.display()));

        let mut command = hew_command(&repo);
        command.arg("run").arg(&path);
        let output = hew_testutil::run_command_bounded(
            &mut command,
            format!("hew run {}", path.display()),
            hew_testutil::DEFAULT_EXEC_TIMEOUT,
        )
        .expect("spawn hew run");
        assert!(
            output.status.success(),
            "hew run {} exited non-zero (status={:?}); stderr:\n{}",
            fixture.stem,
            output.status,
            String::from_utf8_lossy(&output.stderr)
        );
        let stdout = String::from_utf8(output.stdout).expect("fixture stdout is utf-8");
        assert_eq!(stdout, expected, "{} stdout mismatch", fixture.stem);
    }
}
